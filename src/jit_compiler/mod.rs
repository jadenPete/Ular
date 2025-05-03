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
    builder: &'a Builder<'context>,
    built_in_values: &'a mut BuiltInValues<'context>,
    function: FunctionValue<'context>,
    execution_engine: &'a ExecutionEngine<'context>,
    module: &'a mut UlarModule<'context>,
}

impl<'a, 'context> JitFunctionCompiler<'a, 'context> {
    fn append_basic_block(&self) -> BasicBlock<'context> {
        let name = format!("{}", self.function.count_basic_blocks());

        self.context.append_basic_block(self.function, &name)
    }

    fn compile_block(
        &mut self,
        scope: &mut JitCompilerScope<'_, 'context>,
        basic_block: BasicBlock<'context>,
        block: &TypedBlock,
    ) -> Result<UlarValue<'context>, CompilationError> {
        scope.with_child(basic_block, |child_scope| {
            self.compile_statements_with_functions_hoisted(child_scope, &block.statements)?;

            match &block.result {
                Some(result) => self.compile_expression(child_scope, result),
                None => Ok(UlarValue::Unit),
            }
        })
    }

    fn compile_call(
        &mut self,
        scope: &mut JitCompilerScope<'_, 'context>,
        call: &TypedCall,
    ) -> Result<UlarValue<'context>, CompilationError> {
        let function: UlarFunction = self.compile_identifier(scope, &call.function)?.try_into()?;
        let mut argument_values = Vec::<BasicMetadataValueEnum>::new();

        for argument in &call.arguments {
            let argument_value: IntValue = self.compile_expression(scope, argument)?.try_into()?;

            argument_values.push(argument_value.into());
        }

        let name = scope.get_local_name();
        let call_result =
            match function {
                UlarFunction::DirectReference(inkwell_function) => self.builder.build_direct_call(
                    inkwell_function,
                    &argument_values,
                    &name.to_string(),
                ),

                UlarFunction::IndirectReference { pointer, type_ } => self
                    .builder
                    .build_indirect_call(type_, pointer, &argument_values, &name.to_string()),
            }
            .unwrap();

        UlarValue::from_call_site_value(&self.context, &call.get_type(), call_result)
    }

    fn compile_expression(
        &mut self,
        scope: &mut JitCompilerScope<'_, 'context>,
        expression: &TypedExpression,
    ) -> Result<UlarValue<'context>, CompilationError> {
        match expression {
            TypedExpression::Call(call) => self.compile_call(scope, call),
            TypedExpression::Identifier(identifier) => self.compile_identifier(scope, identifier),
            TypedExpression::If(if_expression) => self.compile_if_expression(scope, if_expression),
            TypedExpression::InfixOperation(infix_operation) => {
                self.compile_infix_operation(scope, infix_operation)
            }

            TypedExpression::Number(number) => Ok(UlarValue::Int(
                number
                    .type_
                    .inkwell_type(self.context)
                    .const_int(number.value as u64, number.type_.is_signed()),
            )),

            TypedExpression::PrefixOperation(prefix_operation) => {
                self.compile_prefix_operation(scope, prefix_operation)
            }
        }
    }

    fn compile_function_definition(
        &mut self,
        scope: &mut JitCompilerScope<'_, 'context>,
        definition: &TypedFunctionDefinition,
    ) -> Result<(), CompilationError> {
        if scope.has_parent() {
            Err(CompilationError::NestedFunctionsNotSupported)
        } else {
            let local_name = scope.get_local_name();
            let function = match scope.get_variable_value(
                &definition.name.0,
                local_name,
                self.context,
                self.builder,
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
            let mut function_scope = JitCompilerScope::new(entry_block, Some(scope));

            for (definition_parameter, function_parameter) in
                definition.parameters.iter().zip(function.get_param_iter())
            {
                function_scope.declare_variable(
                    definition_parameter.underlying.0.clone(),
                    UlarValue::from_basic_value(
                        self.context,
                        &definition_parameter.get_type(),
                        function_parameter,
                    )?,
                );
            }

            let mut function_compiler = JitFunctionCompiler {
                context: self.context,

                /*
                 * Before we compile the function, the position of `self.builder` is unknown. We
                 * need to compile it with its own builder so we don't have to reset
                 * `self.builder`'s to its position before we began compiling the function, which is
                 * impossible insofar as I'm aware.
                 */
                builder: &self.context.create_builder(),
                built_in_values: self.built_in_values,
                function,
                execution_engine: self.execution_engine,
                module: self.module,
            };

            function_compiler.builder.position_at_end(entry_block);
            function_compiler.compile_statements_with_functions_hoisted(
                &mut function_scope,
                &definition.body.statements,
            )?;

            let result_value = match &definition.body.result {
                Some(result_expression) => BasicValueEnum::try_from(
                    function_compiler
                        .compile_expression(&mut function_scope, &result_expression)?,
                )
                .ok(),

                None => None,
            };

            match result_value {
                Some(value) => function_compiler.builder.build_return(Some(&value)),
                None => function_compiler.builder.build_return(None),
            }
            .unwrap();

            Ok(())
        }
    }

    fn compile_identifier(
        &mut self,
        scope: &mut JitCompilerScope<'_, 'context>,
        identifier: &TypedIdentifier,
    ) -> Result<UlarValue<'context>, CompilationError> {
        let name = scope.get_local_name();

        Ok(scope
            .get_variable_value(
                &identifier.underlying.0,
                name,
                self.context,
                self.builder,
                self.built_in_values,
                self.execution_engine,
                self.module,
            )
            .ok_or_else(|| CompilationError::UnknownValue(identifier.underlying.0.clone()))?)
    }

    fn compile_if_expression(
        &mut self,
        scope: &mut JitCompilerScope<'_, 'context>,
        if_expression: &TypedIf,
    ) -> Result<UlarValue<'context>, CompilationError> {
        let condition = self.compile_expression(scope, &if_expression.condition)?;
        let then_block = self.append_basic_block();
        let else_block = self.append_basic_block();

        self.builder
            .build_conditional_branch(condition.try_into()?, then_block, else_block)
            .unwrap();

        let end_block = self.append_basic_block();

        self.builder.position_at_end(then_block);

        let then_value = self.compile_block(scope, then_block, &if_expression.then_block)?;

        self.builder.build_unconditional_branch(end_block).unwrap();
        self.builder.position_at_end(else_block);

        let else_value = self.compile_block(scope, else_block, &if_expression.else_block)?;

        self.builder.build_unconditional_branch(end_block).unwrap();

        scope.basic_block = end_block;

        self.builder.position_at_end(end_block);

        UlarValue::build_phi(
            &self.context,
            self.builder,
            &if_expression.type_,
            &[(then_value, then_block), (else_value, else_block)],
            scope.get_local_name(),
        )
    }

    fn compile_infix_operation(
        &mut self,
        scope: &mut JitCompilerScope<'_, 'context>,
        infix_operation: &TypedInfixOperation,
    ) -> Result<UlarValue<'context>, CompilationError> {
        let left_value: IntValue = self
            .compile_expression(scope, &infix_operation.left)?
            .try_into()?;

        let right_value: IntValue = self
            .compile_expression(scope, &infix_operation.right)?
            .try_into()?;

        let name = scope.get_local_name();

        match infix_operation.operator {
            InfixOperator::Addition => Ok(self
                .builder
                .build_int_add(left_value, right_value, &name.to_string())
                .unwrap()
                .into()),

            InfixOperator::Subtraction => Ok(self
                .builder
                .build_int_sub(left_value, right_value, &name.to_string())
                .unwrap()
                .into()),

            InfixOperator::Multiplication => Ok(self
                .builder
                .build_int_mul(left_value, right_value, &name.to_string())
                .unwrap()
                .into()),

            InfixOperator::Division => match infix_operation.get_type() {
                Type::Numeric(numeric_type) => {
                    self.compile_infix_division(name, numeric_type, left_value, right_value)
                }

                type_ => Err(CompilationError::InternalError(
                    InternalError::JitCompilerExpectedNumericType {
                        actual_type: format!("{}", type_),
                    },
                )),
            },

            InfixOperator::Modulo => match infix_operation.get_type() {
                Type::Numeric(numeric_type) => Ok(if numeric_type.is_signed() {
                    self.builder
                        .build_int_signed_rem(left_value, right_value, &name.to_string())
                } else {
                    self.builder
                        .build_int_signed_rem(left_value, right_value, &name.to_string())
                }
                .unwrap()
                .into()),

                type_ => Err(CompilationError::InternalError(
                    InternalError::JitCompilerExpectedNumericType {
                        actual_type: format!("{}", type_),
                    },
                )),
            },

            InfixOperator::LogicalAnd => Ok(self
                .builder
                .build_and(left_value, right_value, &name.to_string())
                .unwrap()
                .into()),

            InfixOperator::LogicalOr => Ok(self
                .builder
                .build_or(left_value, right_value, &name.to_string())
                .unwrap()
                .into()),
        }
    }

    fn compile_infix_division(
        &mut self,
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
            self.builder
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
        scope: &mut JitCompilerScope<'_, 'context>,
        prefix_operation: &TypedPrefixOperation,
    ) -> Result<UlarValue<'context>, CompilationError> {
        let expression = self.compile_expression(scope, &prefix_operation.expression)?;
        let name = scope.get_local_name();

        match prefix_operation.operator {
            SimplePrefixOperator::Not => Ok(self
                .builder
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
        scope: &mut JitCompilerScope<'_, 'context>,
        statement: &TypedStatement,
    ) -> Result<(), CompilationError> {
        match statement {
            TypedStatement::Expression(expression) => {
                self.compile_expression(scope, expression)?;
            }

            TypedStatement::FunctionDefinition(definition) => {
                self.compile_function_definition(scope, definition)?;
            }

            TypedStatement::VariableDefinition(definition) => {
                let value = self.compile_expression(scope, &definition.value)?;

                if scope.declare_variable(definition.name.0.clone(), value) {
                    return Err(CompilationError::VariableAlreadyDefined(
                        definition.name.0.clone(),
                    ));
                }
            }

            TypedStatement::NoOp => {}
        }

        Ok(())
    }

    fn compile_statements_with_functions_hoisted(
        &mut self,
        scope: &mut JitCompilerScope<'_, 'context>,
        statements: &[TypedStatement],
    ) -> Result<(), CompilationError> {
        for statement in statements {
            if let TypedStatement::FunctionDefinition(definition) = statement {
                let function_type = definition.type_.inkwell_type(self.context)?;
                let function =
                    self.module
                        .underlying
                        .add_function(&definition.name.0, function_type, None);

                if scope.declare_variable(
                    definition.name.0.clone(),
                    UlarValue::Function(UlarFunction::DirectReference(function)),
                ) {
                    return Err(CompilationError::VariableAlreadyDefined(
                        definition.name.0.clone(),
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
                self.compile_function_definition(scope, definition)?;
            }
        }

        for statement in statements {
            if let TypedStatement::FunctionDefinition(_) = statement {
            } else {
                self.compile_statement(scope, statement)?;
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

    let mut scope = JitCompilerScope::new(main_may_throw_entry_block, None);
    let mut function_compiler = JitFunctionCompiler {
        context: &context,
        builder: &builder,
        built_in_values: built_in_values,
        execution_engine: &execution_engine,
        function: main_may_throw_function,
        module,
    };

    function_compiler.compile_statements_with_functions_hoisted(&mut scope, &program.statements)?;

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
