mod built_in_values;
mod module;
mod scope;
mod value;

use crate::{
    error::CompilationError,
    jit_compiler::{
        built_in_values::BuiltInValues,
        module::UlarModule,
        scope::JitCompilerScope,
        value::{UlarFunction, UlarValue},
    },
    parser::program::Operator,
    typechecker::typed_program::{
        Typed, TypedBlock, TypedCall, TypedExpression, TypedIdentifier, TypedIf, TypedInfix,
        TypedNumber, TypedProgram, TypedStatement,
    },
};

use inkwell::{
    basic_block::BasicBlock,
    builder::Builder,
    context::Context,
    execution_engine::{ExecutionEngine, JitFunction},
    values::{AnyValue, BasicMetadataValueEnum, FunctionValue, IntValue},
    OptimizationLevel,
};

use log::debug;

type MainFunction = unsafe extern "C" fn();

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
            for statement in &block.statements {
                self.compile_statement(child_scope, statement)?;
            }

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

        UlarValue::from_call_site_value(
            &self.context,
            call.get_type(),
            self.builder
                .build_indirect_call(
                    function.type_,
                    function.pointer,
                    &argument_values,
                    &scope.get_local_name().to_string(),
                )
                .unwrap(),
        )
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
            TypedExpression::Infix(infix) => self.compile_infix(scope, infix),
            TypedExpression::Number(TypedNumber(number)) => Ok(self
                .context
                .i32_type()
                .const_int(number.0 as u64, true)
                .into()),
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
            if_expression.type_.clone(),
            &[(then_value, then_block), (else_value, else_block)],
            scope.get_local_name(),
        )
    }

    fn compile_infix(
        &mut self,
        scope: &mut JitCompilerScope<'_, 'context>,
        infix: &TypedInfix,
    ) -> Result<UlarValue<'context>, CompilationError> {
        let left_value: IntValue = self.compile_expression(scope, &infix.left)?.try_into()?;
        let right_value: IntValue = self.compile_expression(scope, &infix.right)?.try_into()?;
        let name = scope.get_local_name();

        Ok(match infix.operator {
            Operator::Addition => {
                self.builder
                    .build_int_add(left_value, right_value, &name.to_string())
            }

            Operator::Subtraction => {
                self.builder
                    .build_int_sub(left_value, right_value, &name.to_string())
            }

            Operator::Multiplication => {
                self.builder
                    .build_int_mul(left_value, right_value, &name.to_string())
            }

            Operator::Division => {
                self.builder
                    .build_int_signed_div(left_value, right_value, &name.to_string())
            }

            Operator::Modulo => {
                self.builder
                    .build_int_signed_rem(left_value, right_value, &name.to_string())
            }

            Operator::LogicalAnd => {
                self.builder
                    .build_and(left_value, right_value, &name.to_string())
            }

            Operator::LogicalOr => {
                self.builder
                    .build_or(left_value, right_value, &name.to_string())
            }
        }
        .unwrap()
        .into())
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
}

fn compile_program<'a>(
    context: &'a Context,
    builder: &Builder<'a>,
    built_in_values: &mut BuiltInValues<'a>,
    module: &mut UlarModule<'a>,
    execution_engine: &ExecutionEngine<'a>,
    program: &TypedProgram,
    print_to_stderr: bool,
) -> Result<JitFunction<'a, MainFunction>, CompilationError> {
    let main_function =
        module
            .underlying
            .add_function("main", context.void_type().fn_type(&[], false), None);

    let entry_block = context.append_basic_block(main_function, "entry");

    builder.position_at_end(entry_block);

    let mut scope = JitCompilerScope::new(entry_block, None);
    let mut function_compiler = JitFunctionCompiler {
        context: &context,
        builder: &builder,
        built_in_values: built_in_values,
        execution_engine: &execution_engine,
        function: main_function,
        module,
    };

    for statement in program.statements.iter() {
        function_compiler.compile_statement(&mut scope, &statement)?;
    }

    builder.build_return(None).unwrap();

    if print_to_stderr {
        debug!("Output of the jit_compiler phase:");
        debug!("{}", main_function.print_to_string().to_string());
    }

    Ok(unsafe { execution_engine.get_function("main").unwrap() })
}

pub fn compile_and_execute_program(
    program: &TypedProgram,
    print_to_stderr: bool,
) -> Result<(), CompilationError> {
    let context = Context::create();
    let builder = context.create_builder();
    let mut built_in_values = BuiltInValues::new(&context);
    let mut module: UlarModule = context.create_module("main").into();
    let execution_engine = module
        .underlying
        .create_jit_execution_engine(OptimizationLevel::None)
        .unwrap();

    let main_function = compile_program(
        &context,
        &builder,
        &mut built_in_values,
        &mut module,
        &execution_engine,
        program,
        print_to_stderr,
    )?;

    unsafe {
        main_function.call();
    }

    Ok(())
}
