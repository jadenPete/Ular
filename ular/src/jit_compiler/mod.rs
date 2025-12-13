mod function;
pub mod module;
mod scope;
mod value;

use crate::{
    arguments::{GarbageCollectionPlan, PhaseName},
    dependency_analyzer::analyzed_program::AnalyzedProgram,
    error_reporting::{CompilationError, CompilationErrorMessage},
    jit_compiler::{
        function::JitFunctionCompiler,
        module::StructInformation,
        module::{
            built_in_values::{BuiltInFunction, BuiltInValue, JitCompilerBuiltInValues},
            fork_function_cache::ForkFunctionCache,
            memory_manager::UlarMemoryManager,
            UlarModule,
        },
        scope::{JitCompilerScope, JitCompilerScopeContext},
        value::UlarValue,
    },
    mmtk::{
        object_descriptor_store::{ObjectDescriptorStore, STRING_LITERAL_DESCRIPTOR_INDEX},
        runtime::mmtk_set_object_descriptor_store,
    },
    parser::program::Node,
    phase::Phase,
};
use inkwell::{
    builder::Builder,
    context::Context,
    execution_engine::{ExecutionEngine, JitFunction},
    passes::PassBuilderOptions,
    targets::{CodeModel, RelocMode, Target, TargetData, TargetMachine},
    types::{ArrayType, BasicType},
    values::FunctionValue,
    AddressSpace, OptimizationLevel,
};
use std::{
    collections::HashMap,
    fmt::{Debug, Formatter},
};
use ular_scheduler::VALUE_BUFFER_WORD_SIZE;

const MAIN_HARNESS_FUNCTION_NAME: &str = "main_harness";
const MAIN_FUNCTION_NAME: &str = "main";

type MainFunction = unsafe extern "C" fn() -> u8;

pub(crate) struct ExecutorPhase;

impl Phase<&CompiledProgram<'_>> for ExecutorPhase {
    type Output = u8;

    fn execute(&self, program: &CompiledProgram) -> Result<u8, CompilationError> {
        Ok(unsafe { program.main_harness_function.call() })
    }

    fn name() -> PhaseName {
        PhaseName::Executor
    }
}

pub(crate) struct CompiledProgram<'a> {
    module: UlarModule<'a>,
    main_harness_function: JitFunction<'a, unsafe extern "C" fn() -> u8>,
}

impl Debug for CompiledProgram<'_> {
    fn fmt(&self, formatter: &mut Formatter<'_>) -> std::fmt::Result {
        write!(
            formatter,
            "{}",
            self.module.underlying.print_to_string().to_str().unwrap()
        )
    }
}

pub(crate) struct JitCompilerPhase<'a> {
    pub(crate) context: &'a Context,
    pub(crate) garbage_collection_plan: GarbageCollectionPlan,
    pub(crate) print_stack_map: bool,
    pub(crate) additional_values: HashMap<String, Box<dyn BuiltInValue<'a> + 'a>>,
}

impl<'a> JitCompilerPhase<'a> {
    #[allow(clippy::too_many_arguments)]
    fn compile_main_function(
        &self,
        builder: &Builder<'a>,
        built_in_values: &JitCompilerBuiltInValues<'a>,
        execution_engine: &ExecutionEngine<'a>,
        fork_function_cache: &ForkFunctionCache<'a>,
        module: &UlarModule<'a>,
        struct_information: &[StructInformation<'_, 'a>],
        program: &AnalyzedProgram,
    ) -> Result<FunctionValue<'a>, CompilationError> {
        let main_function = module.add_garbage_collecting_function(
            MAIN_FUNCTION_NAME,
            self.context.void_type().fn_type(
                &[self.context.ptr_type(AddressSpace::default()).into()],
                false,
            ),
            None,
        );

        let worker = main_function
            .get_first_param()
            .unwrap()
            .into_pointer_value();

        let main_entry_block = self.context.append_basic_block(main_function, "entry");

        builder.position_at_end(main_entry_block);

        let struct_method_values = program
            .structs
            .iter()
            .map(|struct_definition| {
                struct_definition
                    .methods
                    .iter()
                    .map(|method_definition| {
                        let function_type = method_definition
                            .type_
                            .inkwell_type(self.context)
                            .ok_or_else(|| CompilationError {
                                message: CompilationErrorMessage::UnitPassedAsValue,
                                position: Some(method_definition.get_position()),
                            })?;

                        let function_name = format!(
                            "{}_{}",
                            struct_definition.name.value, method_definition.name.value
                        );

                        Ok(module.add_garbage_collecting_function(
                            &function_name,
                            function_type,
                            None,
                        ))
                    })
                    .collect::<Result<_, _>>()
            })
            .collect::<Result<_, _>>()?;

        let function_values = program
            .functions
            .iter()
            .map(|definition| {
                let function_type =
                    definition.type_.inkwell_type(self.context).ok_or_else(|| {
                        CompilationError {
                            message: CompilationErrorMessage::UnitPassedAsValue,
                            position: Some(definition.get_position()),
                        }
                    })?;

                Ok(module.add_garbage_collecting_function(
                    &definition.name.value,
                    function_type,
                    None,
                ))
            })
            .collect::<Result<_, _>>()?;

        let string_values =
            self.get_string_values(module, execution_engine.get_target_data(), program);

        let scope_context = JitCompilerScopeContext {
            function_values,
            string_values,
            struct_method_values,
        };

        let mut scope = JitCompilerScope::new_without_parent(None, worker);
        let mut function_compiler = JitFunctionCompiler {
            built_in_values,
            context: self.context,
            execution_engine,
            fork_function_cache,
            module,
            scope_context: &scope_context,
            struct_information,
            function: main_function,
        };

        for (struct_definition, method_values) in program
            .structs
            .iter()
            .zip(&scope_context.struct_method_values)
        {
            for (method_definition, method_value) in
                struct_definition.methods.iter().zip(method_values)
            {
                function_compiler.compile_function_definition(
                    &mut scope,
                    method_definition,
                    *method_value,
                )?;
            }
        }

        for (definition, value) in program.functions.iter().zip(&scope_context.function_values) {
            function_compiler.compile_function_definition(&mut scope, definition, *value)?;
        }

        function_compiler.compile_expression_graph(
            builder,
            &mut scope,
            &program.expression_graph,
        )?;

        builder.build_return(None).unwrap();

        Ok(main_function)
    }

    /// Generates the main harness function, which wraps the main function.
    ///
    /// This function is responsible for:
    /// 1. Setting up the thread pool and creating a worker
    /// 2. Invoking the main function
    /// 3. Handling exceptions that may occur during the execution of the main function
    ///
    /// The LLVM IR generated by [compile_main_harness_function] should look like this:
    ///
    /// ```llvm
    /// define i8 @main_harness() personality ptr @__gxx_personality_v0 {
    /// entry:
    ///     %worker_pool = call ptr @_workerpool_new()
    ///     %worker = call ptr @_workerpool_worker(ptr %worker_pool)
    ///
    ///     invoke void @main(%worker) to label %then unwind label %catch
    ///
    /// catch:
    ///     %landing_pad_result = landingpad { ptr, i32 } catch ptr null
    ///     %exception_structure = extractvalue { ptr, i32 } %landing_pad_result, 0
    ///     %exception_object = call ptr @__cxa_begin_catch(ptr %exception_structure)
    ///     %exception_value = load ptr, ptr %exception_object
    ///
    ///     call void @_print_c_string(ptr %exception_value)
    ///     call void @__cxa_end_catch()
    ///     br label %end
    ///
    /// end:
    ///     %return_value = phi i8 [ 0, %entry ], [ 1, %catch ]
    ///
    ///     call void @_worker_free(ptr %worker)
    ///     call void @_workerpool_join(ptr %worker_pool)
    ///     ret i8 %return_value
    /// }
    /// ```
    ///
    /// This function ensures that if `main` completes successfully, it returns 0.
    /// If an exception is thrown, it prints the exception and returns 1.
    fn compile_main_harness_function(
        &self,
        builder: &Builder<'a>,
        built_in_values: &JitCompilerBuiltInValues<'a>,
        execution_engine: &ExecutionEngine<'a>,
        module: &UlarModule<'a>,
        main_function: FunctionValue<'a>,
    ) {
        let main_harness_function = module.add_garbage_collecting_function(
            MAIN_HARNESS_FUNCTION_NAME,
            self.context.i8_type().fn_type(&[], false),
            None,
        );

        let entry_block = self
            .context
            .append_basic_block(main_harness_function, "entry");

        let catch_block = self
            .context
            .append_basic_block(main_harness_function, "catch");

        let end_block = self
            .context
            .append_basic_block(main_harness_function, "end");

        builder.position_at_end(entry_block);
        builder
            .build_call(
                built_in_values._mmtk_init.get_inkwell_function(
                    self.context,
                    execution_engine,
                    module,
                ),
                &[built_in_values
                    ._garbage_collection_plan_type
                    .const_int(self.garbage_collection_plan as u64, false)
                    .into()],
                "",
            )
            .unwrap();

        let worker_pool = builder
            .build_call(
                built_in_values._workerpool_new.get_inkwell_function(
                    self.context,
                    execution_engine,
                    module,
                ),
                &[],
                "worker_pool",
            )
            .unwrap()
            .try_as_basic_value()
            .unwrap_left()
            .into();

        let worker = builder
            .build_call(
                built_in_values._workerpool_worker.get_inkwell_function(
                    self.context,
                    execution_engine,
                    module,
                ),
                &[worker_pool],
                "worker",
            )
            .unwrap()
            .try_as_basic_value()
            .unwrap_left();

        builder
            .build_call(
                built_in_values
                    ._mmtk_bind_current_mutator
                    .get_inkwell_function(self.context, execution_engine, module),
                &[],
                "",
            )
            .unwrap();

        builder
            .build_invoke(main_function, &[worker], end_block, catch_block, "")
            .unwrap();

        builder.position_at_end(catch_block);

        let landing_pad_result_type = self.context.struct_type(
            &[
                self.context.ptr_type(AddressSpace::default()).into(),
                self.context.i32_type().into(),
            ],
            false,
        );

        let landing_pad_result = builder
            .build_landing_pad(
                landing_pad_result_type,
                built_in_values.__gxx_personality_v0.get_inkwell_function(
                    self.context,
                    execution_engine,
                    module,
                ),
                &[self
                    .context
                    .ptr_type(AddressSpace::default())
                    .const_zero()
                    .into()],
                false,
                "landing_pad_result",
            )
            .unwrap()
            .into_struct_value();

        let exception_structure = builder
            .build_extract_value(landing_pad_result, 0, "exception_structure")
            .unwrap();

        let exception_object = builder
            .build_call(
                built_in_values.__cxa_begin_catch.get_inkwell_function(
                    self.context,
                    execution_engine,
                    module,
                ),
                &[exception_structure.into()],
                "exception_object",
            )
            .unwrap()
            .try_as_basic_value()
            .unwrap_left()
            .into_pointer_value();

        let exception_value = builder
            .build_load(
                self.context.ptr_type(AddressSpace::default()),
                exception_object,
                "exception_value",
            )
            .unwrap();

        builder
            .build_call(
                built_in_values._print_c_string.get_inkwell_function(
                    self.context,
                    execution_engine,
                    module,
                ),
                &[exception_value.into()],
                "",
            )
            .unwrap();

        builder
            .build_call(
                built_in_values.__cxa_end_catch.get_inkwell_function(
                    self.context,
                    execution_engine,
                    module,
                ),
                &[],
                "",
            )
            .unwrap();

        builder.build_unconditional_branch(end_block).unwrap();
        builder.position_at_end(end_block);

        let phi = builder
            .build_phi(self.context.i8_type(), "return_value")
            .unwrap();

        phi.add_incoming(&[
            (&self.context.i8_type().const_int(0, false), entry_block),
            (&self.context.i8_type().const_int(1, false), catch_block),
        ]);

        builder
            .build_call(
                built_in_values._worker_free.get_inkwell_function(
                    self.context,
                    execution_engine,
                    module,
                ),
                &[worker.into()],
                "",
            )
            .unwrap();

        builder
            .build_call(
                built_in_values._workerpool_join.get_inkwell_function(
                    self.context,
                    execution_engine,
                    module,
                ),
                &[worker_pool],
                "",
            )
            .unwrap();

        builder.build_return(Some(&phi.as_basic_value())).unwrap();
    }

    fn compile_program(
        &self,
        built_in_values: &JitCompilerBuiltInValues<'a>,
        execution_engine: &ExecutionEngine<'a>,
        fork_function_cache: &ForkFunctionCache<'a>,
        module: &UlarModule<'a>,
        program: &AnalyzedProgram,
    ) -> Result<JitFunction<'a, MainFunction>, CompilationError> {
        let target_triple = TargetMachine::get_default_triple();
        let target = Target::from_triple(&target_triple).unwrap();
        let target_machine = target
            .create_target_machine(
                &target_triple,
                "generic",
                "",
                OptimizationLevel::None,
                RelocMode::Default,
                CodeModel::JITDefault,
            )
            .unwrap();

        let target_data = target_machine.get_target_data();
        let mut object_descriptor_store = ObjectDescriptorStore::new(self.context, &target_data);
        let struct_types = program
            .structs
            .iter()
            .map(|struct_definition| {
                let struct_type = self
                    .context
                    .opaque_struct_type(&struct_definition.name.value);

                let mut field_types = Vec::with_capacity(struct_definition.fields.len() + 1);

                // The first field is the descriptor reference
                field_types.push(self.context.i32_type().as_basic_type_enum());

                for field in &struct_definition.fields {
                    field_types.push(field.type_.inkwell_type(self.context).ok_or_else(|| {
                        CompilationError {
                            message: CompilationErrorMessage::UnitPassedAsValue,
                            position: Some(field.name.get_position()),
                        }
                    })?);
                }

                struct_type.set_body(&field_types, false);

                Ok(struct_type)
            })
            .collect::<Result<Vec<_>, _>>()?;

        let struct_information = program
            .structs
            .iter()
            .enumerate()
            .map(|(i, struct_definition)| {
                Ok(StructInformation {
                    definition: struct_definition,
                    inkwell_type: struct_types[i],
                    descriptor_reference: object_descriptor_store
                        .get_or_set_descriptor_for_struct(
                            program,
                            &struct_types,
                            &target_data,
                            i,
                        )?,
                })
            })
            .collect::<Result<Vec<_>, _>>()?;

        mmtk_set_object_descriptor_store(object_descriptor_store).unwrap();

        let builder = self.context.create_builder();
        let main_function = self.compile_main_function(
            &builder,
            built_in_values,
            execution_engine,
            fork_function_cache,
            module,
            &struct_information,
            program,
        )?;

        self.compile_main_harness_function(
            &builder,
            built_in_values,
            execution_engine,
            module,
            main_function,
        );

        module
            .underlying
            .run_passes(
                "rewrite-statepoints-for-gc",
                &target_machine,
                PassBuilderOptions::create(),
            )
            .unwrap();

        Ok(unsafe {
            execution_engine
                .get_function(MAIN_HARNESS_FUNCTION_NAME)
                .unwrap()
        })
    }

    fn get_string_values(
        &self,
        module: &UlarModule<'a>,
        target_data: &TargetData,
        program: &AnalyzedProgram,
    ) -> Vec<UlarValue<'a>> {
        let ptr_sized_int_type = self.context.ptr_sized_int_type(target_data, None);

        program
            .string_literals
            .iter()
            .enumerate()
            .map(|(i, string)| {
                let string_array = self.context.const_string(string.as_bytes(), false);
                let string_type = self.context.struct_type(
                    &[
                        self.context.i32_type().into(), // The object descriptor reference
                        ptr_sized_int_type.into(),      // The string length
                        string_array.get_type().into(), // The string data
                    ],
                    false,
                );

                let object_descriptor_reference_value = self
                    .context
                    .i32_type()
                    .const_int(STRING_LITERAL_DESCRIPTOR_INDEX.0.into(), false);

                let length_value = ptr_sized_int_type.const_int(string.len() as u64, false);
                let string_value = string_type.const_named_struct(&[
                    object_descriptor_reference_value.into(),
                    length_value.into(),
                    string_array.into(),
                ]);

                let string_global = module.add_global(string_type);

                string_global.set_name(&format!(".str{}", i));
                string_global.set_constant(true);
                string_global.set_initializer(&string_value);

                UlarValue::String(string_global.as_pointer_value())
            })
            .collect()
    }
}

impl<'a> Phase<&AnalyzedProgram> for JitCompilerPhase<'a> {
    type Output = CompiledProgram<'a>;

    fn execute(&self, program: &AnalyzedProgram) -> Result<CompiledProgram<'a>, CompilationError> {
        let module: UlarModule = self.context.create_module("main").into();

        // SAFETY: `memory_manager` is passed directly to
        // `Module::create_mcjit_execution_engine_with_memory_manager`, which we assume doesn't use
        // the allocated code or data sections after `UlarMemoryManager::destroy` is called
        let memory_manager = unsafe { UlarMemoryManager::new(self.print_stack_map) };
        let execution_engine = module
            .underlying
            .create_mcjit_execution_engine_with_memory_manager(
                memory_manager,
                OptimizationLevel::None,
                CodeModel::JITDefault,
                false,
                true,
            )
            .unwrap();

        let built_in_values = JitCompilerBuiltInValues::new(
            self.context,
            &execution_engine,
            self.additional_values.clone(),
        );

        let fork_function_cache = ForkFunctionCache::new();
        let main_harness_function = self.compile_program(
            &built_in_values,
            &execution_engine,
            &fork_function_cache,
            &module,
            program,
        )?;

        Ok(CompiledProgram {
            module,
            main_harness_function,
        })
    }

    fn name() -> PhaseName {
        PhaseName::JitCompiler
    }
}

fn get_value_buffer_type(context: &Context) -> ArrayType {
    context.i64_type().array_type(VALUE_BUFFER_WORD_SIZE as u32)
}
