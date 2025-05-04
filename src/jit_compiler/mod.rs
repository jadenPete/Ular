mod built_in_values;
mod module;
mod scope;
mod value;

use crate::{
    error::{CompilationError, InternalError},
    jit_compiler::{
        built_in_values::BuiltInValues,
        module::UlarModule,
        scope::{JitCompilerScope, LocalName},
        value::{UlarFunction, UlarValue},
    },
    parser::{
        program::InfixOperator,
        type_::{NumericType, Type},
    },
    simplifier::simple_program::SimplePrefixOperator,
    typechecker::typed_program::{
        Typed, TypedBlock, TypedCall, TypedExpression, TypedFunctionDefinition, TypedIdentifier,
        TypedIf, TypedInfixOperation, TypedPrefixOperation, TypedProgram, TypedStatement,
    },
};

use built_in_values::BuiltInFunction;
use inkwell::{
    basic_block::BasicBlock,
    builder::Builder,
    context::Context,
    execution_engine::{ExecutionEngine, JitFunction},
    values::{BasicMetadataValueEnum, BasicValueEnum, FunctionValue, IntValue},
    AddressSpace, OptimizationLevel,
};

use log::debug;

type MainFunction = unsafe extern "C" fn() -> u8;

struct JitFunctionCompiler<'a, 'context> {
    context: &'context Context,
    built_in_values: &'a mut BuiltInValues<'context>,
    function: FunctionValue<'context>,
    execution_engine: &'a ExecutionEngine<'context>,
    module: &'a mut UlarModule<'context>,
}

impl<'a, 'context> JitFunctionCompiler<'a, 'context> {
    fn append_basic_block(
        &self,
        scope: &mut JitCompilerScope<'_, 'context>,
    ) -> BasicBlock<'context> {
        self.context
            .append_basic_block(self.function, &scope.get_local_name().to_string())
    }

    fn compile_block(
        &mut self,
        builder: &Builder<'context>,
        scope: &mut JitCompilerScope<'_, 'context>,
        block: &TypedBlock,
    ) -> Result<UlarValue<'context>, CompilationError> {
        scope.with_child(|child_scope| {
            self.compile_statements_with_functions_hoisted(
                builder,
                child_scope,
                &block.statements,
            )?;

            match &block.result {
                Some(result) => self.compile_expression(builder, child_scope, result),
                None => Ok(UlarValue::Unit),
            }
        })
    }

    fn compile_call(
        &mut self,
        builder: &Builder<'context>,
        scope: &mut JitCompilerScope<'_, 'context>,
        call: &TypedCall,
    ) -> Result<UlarValue<'context>, CompilationError> {
        let function: UlarFunction = self
            .compile_expression(builder, scope, &call.function)?
            .try_into()?;

        let mut argument_values = Vec::<BasicMetadataValueEnum>::new();

        for argument in &call.arguments {
            let argument_value: IntValue = self
                .compile_expression(builder, scope, argument)?
                .try_into()?;

            argument_values.push(argument_value.into());
        }

        let name = scope.get_local_name();
        let call_result = match function {
            UlarFunction::DirectReference(inkwell_function) => {
                builder.build_direct_call(inkwell_function, &argument_values, &name.to_string())
            }

            UlarFunction::IndirectReference { pointer, type_ } => {
                builder.build_indirect_call(type_, pointer, &argument_values, &name.to_string())
            }
        }
        .unwrap();

        UlarValue::from_call_site_value(&self.context, &call.get_type(), call_result)
    }

    fn compile_expression(
        &mut self,
        builder: &Builder<'context>,
        scope: &mut JitCompilerScope<'_, 'context>,
        expression: &TypedExpression,
    ) -> Result<UlarValue<'context>, CompilationError> {
        match expression {
            TypedExpression::Call(call) => self.compile_call(builder, scope, call),
            TypedExpression::Identifier(identifier) => {
                self.compile_identifier(builder, scope, identifier)
            }

            TypedExpression::If(if_expression) => {
                self.compile_if_expression(builder, scope, if_expression)
            }

            TypedExpression::InfixOperation(infix_operation) => {
                self.compile_infix_operation(builder, scope, infix_operation)
            }

            TypedExpression::Number(number) => Ok(UlarValue::Int(
                number
                    .type_
                    .inkwell_type(self.context)
                    .const_int(number.value as u64, number.type_.is_signed()),
            )),

            TypedExpression::PrefixOperation(prefix_operation) => {
                self.compile_prefix_operation(builder, scope, prefix_operation)
            }
        }
    }

    fn compile_function_definition(
        &mut self,
        builder: &Builder<'context>,
        scope: &mut JitCompilerScope<'_, 'context>,
        definition: &TypedFunctionDefinition,
    ) -> Result<(), CompilationError> {
        if scope.has_parent() {
            Err(CompilationError::NestedFunctionsNotSupported)
        } else {
            let local_name = scope.get_local_name();
            let function = match scope.get_variable_value(
                &definition.name.value,
                local_name,
                self.context,
                builder,
                self.built_in_values,
                self.execution_engine,
                self.module,
            ) {
                Some(UlarValue::Function(UlarFunction::DirectReference(inkwell_function))) => {
                    inkwell_function
                }

                _ => {
                    return Err(CompilationError::InternalError(
                        InternalError::JitCompilerFunctionNotHoisted,
                    ))
                }
            };

            let entry_block = self.context.append_basic_block(function, "entry");
            let mut function_scope = JitCompilerScope::new(Some(scope));

            for (definition_parameter, function_parameter) in
                definition.parameters.iter().zip(function.get_param_iter())
            {
                function_scope.declare_variable(
                    definition_parameter.underlying.value.clone(),
                    UlarValue::from_basic_value(
                        self.context,
                        &definition_parameter.get_type(),
                        function_parameter,
                    )?,
                );
            }

            let mut function_compiler = JitFunctionCompiler {
                context: self.context,
                built_in_values: self.built_in_values,
                function,
                execution_engine: self.execution_engine,
                module: self.module,
            };

            /*
             * Before we compile the function, the position of `builder` is unknown. We need to
             * compile it with its own builder so we don't have to reset `builder`'s to its position
             * before we began compiling the function, which is impossible insofar as I'm aware.
             */
            let function_builder = self.context.create_builder();

            function_builder.position_at_end(entry_block);
            function_compiler.compile_statements_with_functions_hoisted(
                &function_builder,
                &mut function_scope,
                &definition.body.statements,
            )?;

            let result_value = match &definition.body.result {
                Some(result_expression) => {
                    BasicValueEnum::try_from(function_compiler.compile_expression(
                        &function_builder,
                        &mut function_scope,
                        &result_expression,
                    )?)
                    .ok()
                }

                None => None,
            };

            match result_value {
                Some(value) => function_builder.build_return(Some(&value)),
                None => function_builder.build_return(None),
            }
            .unwrap();

            Ok(())
        }
    }

    fn compile_identifier(
        &mut self,
        builder: &Builder<'context>,
        scope: &mut JitCompilerScope<'_, 'context>,
        identifier: &TypedIdentifier,
    ) -> Result<UlarValue<'context>, CompilationError> {
        let name = scope.get_local_name();

        Ok(scope
            .get_variable_value(
                &identifier.underlying.value,
                name,
                self.context,
                builder,
                self.built_in_values,
                self.execution_engine,
                self.module,
            )
            .ok_or_else(|| CompilationError::UnknownValue(identifier.underlying.value.clone()))?)
    }

    fn compile_if_expression(
        &mut self,
        builder: &Builder<'context>,
        scope: &mut JitCompilerScope<'_, 'context>,
        if_expression: &TypedIf,
    ) -> Result<UlarValue<'context>, CompilationError> {
        let condition = self.compile_expression(builder, scope, &if_expression.condition)?;
        let then_block = self.append_basic_block(scope);
        let else_block = self.append_basic_block(scope);

        builder
            .build_conditional_branch(condition.try_into()?, then_block, else_block)
            .unwrap();

        /*
         * Apparently order matters to LLVM, and basic blocks we reference in the phi instruction
         * need to be defined before that instruction is built. If the "then" and/or "else" clauses
         * of this if expression branch and the "end" basic block depends on the result of those
         * nested branches, then the basic blocks to which those nested branches correspond need to
         * be defined before the "end" basic block.
         */
        let then_builder = self.context.create_builder();

        then_builder.position_at_end(then_block);

        let then_value = self.compile_block(&then_builder, scope, &if_expression.then_block)?;
        let else_builder = self.context.create_builder();

        else_builder.position_at_end(else_block);

        let else_value = self.compile_block(&else_builder, scope, &if_expression.else_block)?;
        let end_block = self.append_basic_block(scope);

        then_builder.build_unconditional_branch(end_block).unwrap();
        else_builder.build_unconditional_branch(end_block).unwrap();
        builder.position_at_end(end_block);

        UlarValue::build_phi(
            &self.context,
            builder,
            &if_expression.type_,
            &[
                (then_value, then_builder.get_insert_block().unwrap()),
                (else_value, else_builder.get_insert_block().unwrap()),
            ],
            scope.get_local_name(),
        )
    }

    fn compile_infix_operation(
        &mut self,
        builder: &Builder<'context>,
        scope: &mut JitCompilerScope<'_, 'context>,
        infix_operation: &TypedInfixOperation,
    ) -> Result<UlarValue<'context>, CompilationError> {
        let left_value: IntValue = self
            .compile_expression(builder, scope, &infix_operation.left)?
            .try_into()?;

        let right_value: IntValue = self
            .compile_expression(builder, scope, &infix_operation.right)?
            .try_into()?;

        let name = scope.get_local_name();

        match infix_operation.operator {
            InfixOperator::Addition => Ok(builder
                .build_int_add(left_value, right_value, &name.to_string())
                .unwrap()
                .into()),

            InfixOperator::Subtraction => Ok(builder
                .build_int_sub(left_value, right_value, &name.to_string())
                .unwrap()
                .into()),

            InfixOperator::Multiplication => Ok(builder
                .build_int_mul(left_value, right_value, &name.to_string())
                .unwrap()
                .into()),

            InfixOperator::Division => match infix_operation.get_type() {
                Type::Numeric(numeric_type) => self.compile_infix_division(
                    builder,
                    name,
                    numeric_type,
                    left_value,
                    right_value,
                ),

                type_ => Err(CompilationError::InternalError(
                    InternalError::JitCompilerExpectedNumericType {
                        actual_type: format!("{}", type_),
                    },
                )),
            },

            InfixOperator::Modulo => match infix_operation.get_type() {
                Type::Numeric(numeric_type) => Ok(if numeric_type.is_signed() {
                    builder.build_int_signed_rem(left_value, right_value, &name.to_string())
                } else {
                    builder.build_int_signed_rem(left_value, right_value, &name.to_string())
                }
                .unwrap()
                .into()),

                type_ => Err(CompilationError::InternalError(
                    InternalError::JitCompilerExpectedNumericType {
                        actual_type: format!("{}", type_),
                    },
                )),
            },

            InfixOperator::LogicalAnd => Ok(builder
                .build_and(left_value, right_value, &name.to_string())
                .unwrap()
                .into()),

            InfixOperator::LogicalOr => Ok(builder
                .build_or(left_value, right_value, &name.to_string())
                .unwrap()
                .into()),
        }
    }

    fn compile_infix_division(
        &mut self,
        builder: &Builder<'context>,
        name: LocalName,
        numeric_type: NumericType,
        left_value: IntValue<'context>,
        right_value: IntValue<'context>,
    ) -> Result<UlarValue<'context>, CompilationError> {
        let division_function = self
            .built_in_values
            .get_division_function_mut(numeric_type)
            .get_inkwell_function(self.context, self.execution_engine, self.module);

        UlarValue::from_call_site_value(
            &self.context,
            &Type::Numeric(numeric_type),
            builder
                .build_call(
                    division_function,
                    &[left_value.into(), right_value.into()],
                    &name.to_string(),
                )
                .unwrap(),
        )
    }

    fn compile_prefix_operation(
        &mut self,
        builder: &Builder<'context>,
        scope: &mut JitCompilerScope<'_, 'context>,
        prefix_operation: &TypedPrefixOperation,
    ) -> Result<UlarValue<'context>, CompilationError> {
        let expression = self.compile_expression(builder, scope, &prefix_operation.expression)?;
        let name = scope.get_local_name();

        match prefix_operation.operator {
            SimplePrefixOperator::Not => Ok(builder
                .build_xor(
                    TryInto::<IntValue>::try_into(expression)?,
                    self.context.i8_type().const_int(1, false),
                    &name.to_string(),
                )
                .unwrap()
                .into()),
        }
    }

    fn compile_statement(
        &mut self,
        builder: &Builder<'context>,
        scope: &mut JitCompilerScope<'_, 'context>,
        statement: &TypedStatement,
    ) -> Result<(), CompilationError> {
        match statement {
            TypedStatement::Expression(expression) => {
                self.compile_expression(builder, scope, expression)?;
            }

            TypedStatement::FunctionDefinition(definition) => {
                self.compile_function_definition(builder, scope, definition)?;
            }

            TypedStatement::VariableDefinition(definition) => {
                let value = self.compile_expression(builder, scope, &definition.value)?;

                if scope.declare_variable(definition.name.value.clone(), value) {
                    return Err(CompilationError::VariableAlreadyDefined(
                        definition.name.value.clone(),
                    ));
                }
            }

            TypedStatement::NoOp { .. } => {}
        }

        Ok(())
    }

    fn compile_statements_with_functions_hoisted(
        &mut self,
        builder: &Builder<'context>,
        scope: &mut JitCompilerScope<'_, 'context>,
        statements: &[TypedStatement],
    ) -> Result<(), CompilationError> {
        for statement in statements {
            if let TypedStatement::FunctionDefinition(definition) = statement {
                let function_type = definition.type_.inkwell_type(self.context)?;
                let function = self.module.underlying.add_function(
                    &definition.name.value,
                    function_type,
                    None,
                );

                if scope.declare_variable(
                    definition.name.value.clone(),
                    UlarValue::Function(UlarFunction::DirectReference(function)),
                ) {
                    return Err(CompilationError::VariableAlreadyDefined(
                        definition.name.value.clone(),
                    ));
                }
            }
        }

        /*
         * Typecheck the functions first, so they don't yet have access to global variables.
         * Functions capturing their environment (i.e. closures) aren't yet supported.
         */
        for statement in statements {
            if let TypedStatement::FunctionDefinition(definition) = statement {
                self.compile_function_definition(builder, scope, definition)?;
            }
        }

        for statement in statements {
            if let TypedStatement::FunctionDefinition(_) = statement {
            } else {
                self.compile_statement(builder, scope, statement)?;
            }
        }

        Ok(())
    }
}

fn compile_program<'a>(
    context: &'a Context,
    built_in_values: &mut BuiltInValues<'a>,
    module: &mut UlarModule<'a>,
    execution_engine: &ExecutionEngine<'a>,
    program: &TypedProgram,
    print_to_stderr: bool,
) -> Result<JitFunction<'a, MainFunction>, CompilationError> {
    let main_function =
        module
            .underlying
            .add_function("main", context.i8_type().fn_type(&[], false), None);

    let main_may_throw_function = module.underlying.add_function(
        "main_may_throw",
        context.void_type().fn_type(&[], false),
        None,
    );

    let main_entry_block = context.append_basic_block(main_function, "entry");
    let main_then_block = context.append_basic_block(main_function, "then");
    let main_catch_block = context.append_basic_block(main_function, "catch");
    let builder = context.create_builder();

    builder.position_at_end(main_entry_block);
    builder
        .build_invoke(
            main_may_throw_function,
            &[],
            main_then_block,
            main_catch_block,
            "",
        )
        .unwrap();

    builder.position_at_end(main_then_block);

    let then_return_value = context.i8_type().const_int(0, false);

    builder.build_return(Some(&then_return_value)).unwrap();
    builder.position_at_end(main_catch_block);

    let landing_pad_result_type = context.struct_type(
        &[
            context.ptr_type(AddressSpace::default()).into(),
            context.i32_type().into(),
        ],
        false,
    );

    let landing_pad_result = builder
        .build_landing_pad(
            landing_pad_result_type,
            built_in_values.__gxx_personality_v0.get_inkwell_function(
                context,
                execution_engine,
                module,
            ),
            &[context
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
                context,
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
            context.ptr_type(AddressSpace::default()),
            exception_object,
            "exception_value",
        )
        .unwrap();

    builder
        .build_call(
            built_in_values
                ._print_c_string
                .get_inkwell_function(context, execution_engine, module),
            &[exception_value.into()],
            "",
        )
        .unwrap();

    builder
        .build_call(
            built_in_values
                .__cxa_end_catch
                .get_inkwell_function(context, execution_engine, module),
            &[],
            "",
        )
        .unwrap();

    let catch_return_value = context.i8_type().const_int(1, false);

    builder.build_return(Some(&catch_return_value)).unwrap();

    let main_may_throw_entry_block = context.append_basic_block(main_may_throw_function, "entry");

    builder.position_at_end(main_may_throw_entry_block);

    let mut scope = JitCompilerScope::new(None);
    let mut function_compiler = JitFunctionCompiler {
        context: &context,
        built_in_values: built_in_values,
        execution_engine: &execution_engine,
        function: main_may_throw_function,
        module,
    };

    function_compiler.compile_statements_with_functions_hoisted(
        &builder,
        &mut scope,
        &program.statements,
    )?;

    builder.build_return(None).unwrap();

    if print_to_stderr {
        debug!("Output of the jit_compiler phase:");
        debug!("{}", module.underlying.print_to_string().to_string());
    }

    Ok(unsafe { execution_engine.get_function("main").unwrap() })
}

pub fn compile_and_execute_program(
    program: &TypedProgram,
    print_to_stderr: bool,
) -> Result<u8, CompilationError> {
    let context = Context::create();
    let mut built_in_values = BuiltInValues::new(&context);
    let mut module: UlarModule = context.create_module("main").into();
    let execution_engine = module
        .underlying
        .create_jit_execution_engine(OptimizationLevel::None)
        .unwrap();

    let main_function = compile_program(
        &context,
        &mut built_in_values,
        &mut module,
        &execution_engine,
        program,
        print_to_stderr,
    )?;

    Ok(unsafe { main_function.call() })
}
