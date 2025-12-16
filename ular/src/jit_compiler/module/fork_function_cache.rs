use crate::{
    data_structures::cache::Cache,
    error_reporting::CompilationError,
    jit_compiler::{
        compile_inline_call_without_tick, get_value_buffer_type,
        scope::JitCompilerScope,
        value::{UlarFunction, UlarValue},
    },
};
use either::Left;
use inkwell::{
    builder::Builder,
    context::Context,
    module::Module,
    types::{BasicTypeEnum, FunctionType, StructType},
    values::{BasicMetadataValueEnum, BasicValueEnum, FunctionValue, PointerValue},
    AddressSpace,
};
use std::hash::{Hash, Hasher};

pub(in crate::jit_compiler) struct ForkFunction<'a> {
    function: FunctionValue<'a>,
    context_type: StructType<'a>,
    underlying_pointer: Option<PointerValue<'a>>,
}

impl<'a> ForkFunction<'a> {
    pub(in crate::jit_compiler) fn build_context(
        self,
        builder: &Builder<'a>,
        scope: &mut JitCompilerScope<'_, 'a>,
        arguments: &[UlarValue<'a>],
    ) -> Result<PointerValue<'a>, CompilationError> {
        let context_name = scope.get_local_name();
        let context = builder
            .build_alloca(self.context_type, &context_name.to_string())
            .unwrap();

        if let Some(underlying_pointer) = self.underlying_pointer {
            let context_field = builder
                .build_struct_gep(self.context_type, context, 0, &context_name.to_string())
                .unwrap();

            builder
                .build_store(context_field, underlying_pointer)
                .unwrap();
        }

        for (i, argument) in arguments.iter().enumerate() {
            let context_field_index = match self.underlying_pointer {
                Some(_) => i as u32 + 1,
                None => i as u32,
            };

            let context_field_name = scope.get_local_name();
            let context_field = builder
                .build_struct_gep(
                    self.context_type,
                    context,
                    context_field_index,
                    &context_field_name.to_string(),
                )
                .unwrap();

            builder
                .build_store(context_field, BasicValueEnum::try_from(*argument)?)
                .unwrap();
        }

        Ok(context)
    }

    pub(in crate::jit_compiler) fn function(&self) -> FunctionValue<'a> {
        self.function
    }
}

pub(in crate::jit_compiler) struct ForkFunctionCache<'a, 'context> {
    context: &'context Context,
    module: &'a Module<'context>,
    function_cache: Cache<ForkFunctionKey<'context>, ForkFunctionValue<'context>>,
}

impl<'a, 'context> ForkFunctionCache<'a, 'context> {
    fn get_or_insert_value(&self, key: &ForkFunctionKey<'context>) -> ForkFunctionValue<'context> {
        let cache_length = self.function_cache.len();

        self.function_cache.get_or_compute(key, || {
            // Skip the first parameter, which is the worker pointer
            let parameter_types = &key.get_function_type().get_param_types()[1..];
            let mut context_field_types = Vec::new();

            // The scheduler supports passing a context value to the fork function that's called. Our
            // context value will be a struct containing a pointer to the function we're working with
            // (if it isn't known at compile-time) and the arguments it's called with
            if let ForkFunctionKey::Closure { .. } = key {
                context_field_types.push(self.context.ptr_type(AddressSpace::default()).into());
            }

            for &parameter_type in parameter_types {
                context_field_types.push(parameter_type.try_into().unwrap());
            }

            let context_type = self.context.struct_type(&context_field_types, false);
            let value_buffer_type = get_value_buffer_type(self.context);
            let function = self.module.add_function(
                &format!("fork_{}", cache_length),
                value_buffer_type.fn_type(
                    &[
                        self.context.ptr_type(AddressSpace::default()).into(),
                        self.context.ptr_type(AddressSpace::default()).into(),
                    ],
                    false,
                ),
                None,
            );

            let builder = self.context.create_builder();
            let entry_block = self.context.append_basic_block(function, "entry");

            builder.position_at_end(entry_block);

            let parameters = function.get_params();
            let worker_parameter = parameters[0];
            let context_parameter = parameters[1].into_pointer_value();
            let underlying_function = match key {
                ForkFunctionKey::Function { function } => UlarFunction::Function(*function),
                ForkFunctionKey::Closure { type_ } => {
                    let context_pointer_pointer = builder
                        .build_struct_gep(
                            context_type,
                            context_parameter,
                            0,
                            "underlying_function_context_pointer_pointer",
                        )
                        .unwrap();

                    let context_pointer = builder
                        .build_load(
                            self.context.ptr_type(AddressSpace::default()),
                            context_pointer_pointer,
                            "underlying_function_context_pointer",
                        )
                        .unwrap()
                        .into_pointer_value();

                    UlarFunction::Closure {
                        context_pointer,
                        type_: *type_,
                    }
                }
            };

            let arguments: Vec<BasicMetadataValueEnum> = parameter_types
                .iter()
                .enumerate()
                .map(|(i, parameter_type)| {
                    let pointer = builder
                        .build_struct_gep(
                            context_type,
                            context_parameter,
                            match key {
                                ForkFunctionKey::Function { .. } => i as u32,
                                ForkFunctionKey::Closure { .. } => i as u32 + 1,
                            },
                            &format!("{}_pointer", i),
                        )
                        .unwrap();

                    let value = builder
                        .build_load(
                            BasicTypeEnum::try_from(*parameter_type).unwrap(),
                            pointer,
                            &format!("{}_value", i),
                        )
                        .unwrap()
                        .into();

                    value
                })
                .collect();

            let underlying_result = compile_inline_call_without_tick(
                self.context,
                &builder,
                underlying_function,
                worker_parameter.into_pointer_value(),
                arguments,
                "underlying_function_pointer_pointer",
                "underlying_function_pointer",
                "underlying_result",
            )
            .try_as_basic_value();

            let result_buffer_pointer = builder
                .build_alloca(value_buffer_type, "result_buffer_pointer")
                .unwrap();

            if let Left(underlying_result) = underlying_result {
                builder
                    .build_store(result_buffer_pointer, underlying_result)
                    .unwrap();
            }

            let result_buffer = builder
                .build_load(value_buffer_type, result_buffer_pointer, "result_buffer")
                .unwrap();

            builder.build_return(Some(&result_buffer)).unwrap();

            ForkFunctionValue {
                function,
                context_type,
            }
        })
    }

    pub(in crate::jit_compiler) fn get_or_build(
        &self,
        function: UlarFunction<'context>,
    ) -> ForkFunction<'context> {
        let value = self.get_or_insert_value(&ForkFunctionKey::for_function(function));

        ForkFunction {
            function: value.function,
            context_type: value.context_type,
            underlying_pointer: match function {
                UlarFunction::Function(_) => None,
                UlarFunction::Closure {
                    context_pointer, ..
                } => Some(context_pointer),
            },
        }
    }

    pub(in crate::jit_compiler) fn new(
        context: &'context Context,
        module: &'a Module<'context>,
    ) -> Self {
        Self {
            context,
            module,
            function_cache: Cache::new(),
        }
    }
}

#[derive(Clone, Copy, Eq, PartialEq)]
enum ForkFunctionKey<'a> {
    Function { function: FunctionValue<'a> },
    Closure { type_: FunctionType<'a> },
}

impl<'a> ForkFunctionKey<'a> {
    fn for_function(function: UlarFunction<'a>) -> Self {
        match function {
            UlarFunction::Function(function) => ForkFunctionKey::Function { function },
            UlarFunction::Closure { type_, .. } => ForkFunctionKey::Closure { type_ },
        }
    }

    fn get_function_type(&self) -> FunctionType<'a> {
        match self {
            Self::Function { function } => function.get_type(),
            Self::Closure { type_ } => *type_,
        }
    }
}

impl Hash for ForkFunctionKey<'_> {
    fn hash<A: Hasher>(&self, state: &mut A) {
        match self {
            ForkFunctionKey::Function { function } => {
                state.write_u8(0);

                function.get_name().hash(state);
            }

            ForkFunctionKey::Closure { type_ } => {
                state.write_u8(1);

                HashableFunctionType(*type_).hash(state);
            }
        }
    }
}

#[derive(Clone, Copy)]
struct ForkFunctionValue<'a> {
    function: FunctionValue<'a>,
    context_type: StructType<'a>,
}

#[derive(Eq, PartialEq)]
struct HashableFunctionType<'a>(FunctionType<'a>);

/// Although [FunctionType] implements [Eq], it doesn't implement [Hash]. Because it's known to
/// contain a pointer, we can implement a [Hash] implementation for it by reading the raw bytes behind
/// it and hashing that.
impl Hash for HashableFunctionType<'_> {
    fn hash<A: Hasher>(&self, state: &mut A) {
        let function_type_pointer: *const FunctionType = &self.0;

        // SAFETY: We're just reading the raw bytes behind `self.0`. This is safe.
        state.write(unsafe {
            std::slice::from_raw_parts(
                function_type_pointer as *const u8,
                size_of::<FunctionType>(),
            )
        })
    }
}
