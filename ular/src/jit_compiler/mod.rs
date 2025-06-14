mod built_in_values;
mod fork_function_cache;
mod module;
mod scope;
mod value;

use crate::{
    data_structures::graph::DirectedGraph,
    dependency_analyzer::analyzed_program::{
        AnalyzedBlock, AnalyzedCall, AnalyzedExpression, AnalyzedExpressionRef,
        AnalyzedFunctionDefinition, AnalyzedIf, AnalyzedInfixOperation, AnalyzedPrefixOperation,
        AnalyzedProgram,
    },
    error_reporting::{CompilationError, CompilationErrorMessage, InternalError, Position},
    jit_compiler::{
        built_in_values::BuiltInValues,
        fork_function_cache::ForkFunctionCache,
        module::UlarModule,
        scope::JitCompilerScope,
        value::{UlarFunction, UlarValue},
    },
    parser::{
        program::{InfixOperator, Node},
        type_::Type,
    },
    simplifier::simple_program::SimplePrefixOperator,
    typechecker::typed_program::Typed,
};
use built_in_values::BuiltInFunction;
use inkwell::{
    basic_block::BasicBlock,
    builder::Builder,
    context::Context,
    execution_engine::{ExecutionEngine, JitFunction},
    types::ArrayType,
    values::{BasicValueEnum, FunctionValue, IntValue, PointerValue},
    AddressSpace, OptimizationLevel,
};
use log::debug;
use ular_scheduler::VALUE_BUFFER_WORD_SIZE;

const MAIN_HARNESS_FUNCTION_NAME: &str = "main_harness";
const MAIN_FUNCTION_NAME: &str = "main";

type MainFunction = unsafe extern "C" fn() -> u8;

trait CompilableExpression<'a, 'context> {
    fn compile_fork(
        &self,
        compiler: &mut JitFunctionCompiler<'_, 'context>,
        builder: &Builder<'context>,
        scope: &mut JitCompilerScope<'_, 'context>,
    ) -> Result<Box<dyn ForkedExpression<'context> + 'a>, CompilationError>;

    fn compile_inline(
        &self,
        compiler: &mut JitFunctionCompiler<'_, 'context>,
        builder: &Builder<'context>,
        scope: &mut JitCompilerScope<'_, 'context>,
    ) -> Result<UlarValue<'context>, CompilationError>;
}

trait ForkedExpression<'a> {
    fn compile_join(
        &self,
        compiler: &mut JitFunctionCompiler<'_, 'a>,
        builder: &Builder<'a>,
        scope: &mut JitCompilerScope<'_, 'a>,
    ) -> Result<UlarValue<'a>, CompilationError>;
}

trait InlineExpression<'a> {
    fn compile(
        &self,
        compiler: &mut JitFunctionCompiler<'_, 'a>,
        builder: &Builder<'a>,
        scope: &mut JitCompilerScope<'_, 'a>,
    ) -> Result<UlarValue<'a>, CompilationError>;
}

impl<'a, 'context, A: InlineExpression<'context> + Copy + 'a> CompilableExpression<'a, 'context>
    for A
{
    fn compile_fork(
        &self,
        _compiler: &mut JitFunctionCompiler<'_, 'context>,
        _builder: &Builder<'context>,
        _scope: &mut JitCompilerScope<'_, 'context>,
    ) -> Result<Box<dyn ForkedExpression<'context> + 'a>, CompilationError> {
        Ok(Box::new(*self))
    }

    fn compile_inline(
        &self,
        compiler: &mut JitFunctionCompiler<'_, 'context>,
        builder: &Builder<'context>,
        scope: &mut JitCompilerScope<'_, 'context>,
    ) -> Result<UlarValue<'context>, CompilationError> {
        self.compile(compiler, builder, scope)
    }
}

impl<'a, A: InlineExpression<'a>> ForkedExpression<'a> for A {
    fn compile_join(
        &self,
        compiler: &mut JitFunctionCompiler<'_, 'a>,
        builder: &Builder<'a>,
        scope: &mut JitCompilerScope<'_, 'a>,
    ) -> Result<UlarValue<'a>, CompilationError> {
        self.compile(compiler, builder, scope)
    }
}

#[derive(Clone, Copy)]
struct CompilableCall<'a>(&'a AnalyzedCall);

impl<'context> CompilableCall<'_> {
    fn get_function_and_arguments(
        &self,
        compiler: &mut JitFunctionCompiler<'_, 'context>,
        builder: &Builder<'context>,
        scope: &mut JitCompilerScope<'_, 'context>,
    ) -> Result<(UlarFunction<'context>, Vec<UlarValue<'context>>), CompilationError> {
        let function_name = scope.get_local_name();
        let function: UlarFunction = scope
            .get(
                &self.0.function,
                function_name,
                compiler.context,
                builder,
                compiler.built_in_values,
                compiler.execution_engine,
                compiler.module,
            )?
            .try_into()?;

        let mut arguments = Vec::with_capacity(self.0.arguments.len());

        for argument_reference in &self.0.arguments {
            let name = scope.get_local_name();

            arguments.push(scope.get(
                argument_reference,
                name,
                compiler.context,
                builder,
                compiler.built_in_values,
                compiler.execution_engine,
                compiler.module,
            )?);
        }

        Ok((function, arguments))
    }
}

impl<'a, 'context: 'a> CompilableExpression<'a, 'context> for CompilableCall<'a> {
    fn compile_fork(
        &self,
        compiler: &mut JitFunctionCompiler<'_, 'context>,
        builder: &Builder<'context>,
        scope: &mut JitCompilerScope<'_, 'context>,
    ) -> Result<Box<dyn ForkedExpression<'context> + 'a>, CompilationError> {
        let (function, arguments) = self.get_function_and_arguments(compiler, builder, scope)?;
        let fork_function =
            compiler
                .fork_function_cache
                .get_or_build(function, compiler.context, compiler.module);

        let fork_function_pointer = fork_function.function.as_global_value().as_pointer_value();
        let context = fork_function.build_context(builder, scope, &arguments)?;

        let job_name = scope.get_local_name();
        let job = builder
            .build_call(
                compiler.built_in_values._job_new.get_inkwell_function(
                    compiler.context,
                    compiler.execution_engine,
                    compiler.module,
                ),
                &[],
                &job_name.to_string(),
            )
            .unwrap()
            .try_as_basic_value()
            .unwrap_left();

        let job_pointer_name = scope.get_local_name();
        let job_pointer = builder
            .build_alloca(
                compiler.built_in_values._job_type,
                &job_pointer_name.to_string(),
            )
            .unwrap();

        builder.build_store(job_pointer, job).unwrap();
        builder
            .build_call(
                compiler.built_in_values._worker_fork.get_inkwell_function(
                    compiler.context,
                    compiler.execution_engine,
                    compiler.module,
                ),
                &[
                    scope.worker.into(),
                    job_pointer.into(),
                    fork_function_pointer.into(),
                    context.into(),
                ],
                "",
            )
            .unwrap();

        Ok(Box::new(ForkedCall {
            call: self.0,
            function,
            arguments,
            job_pointer,
        }))
    }

    fn compile_inline(
        &self,
        compiler: &mut JitFunctionCompiler<'_, 'context>,
        builder: &Builder<'context>,
        scope: &mut JitCompilerScope<'_, 'context>,
    ) -> Result<UlarValue<'context>, CompilationError> {
        let (function, arguments) = self.get_function_and_arguments(compiler, builder, scope)?;

        compiler.compile_inline_call(
            builder,
            scope,
            function,
            &arguments,
            &self.0.get_type(),
            self.0.get_position(),
        )
    }
}

struct ForkedCall<'a, 'context> {
    call: &'a AnalyzedCall,
    function: UlarFunction<'context>,
    arguments: Vec<UlarValue<'context>>,
    job_pointer: PointerValue<'context>,
}

impl<'context> ForkedExpression<'context> for ForkedCall<'_, 'context> {
    fn compile_join(
        &self,
        compiler: &mut JitFunctionCompiler<'_, 'context>,
        builder: &Builder<'context>,
        scope: &mut JitCompilerScope<'_, 'context>,
    ) -> Result<UlarValue<'context>, CompilationError> {
        let join_result_name = scope.get_local_name();
        let join_result = builder
            .build_call(
                compiler
                    .built_in_values
                    ._worker_try_join
                    .get_inkwell_function(
                        compiler.context,
                        compiler.execution_engine,
                        compiler.module,
                    ),
                &[scope.worker.into(), self.job_pointer.into()],
                &join_result_name.to_string(),
            )
            .unwrap()
            .try_as_basic_value()
            .unwrap_left()
            .into_struct_value();

        let discriminator_name = scope.get_local_name();
        let discriminator = builder
            .build_extract_value(join_result, 0, &discriminator_name.to_string())
            .unwrap()
            .into_int_value();

        let if_some_block = compiler.append_basic_block(scope);
        let if_none_block = compiler.append_basic_block(scope);
        let end_block = compiler.append_basic_block(scope);

        builder
            .build_conditional_branch(discriminator, if_some_block, if_none_block)
            .unwrap();

        let if_some_builder = compiler.context.create_builder();

        if_some_builder.position_at_end(if_some_block);

        let call_type = self.call.get_type();
        let if_some_result = match self.function.get_type().get_return_type() {
            Some(return_type) => {
                let value_buffer_name = scope.get_local_name();
                let value_buffer = if_some_builder
                    .build_extract_value(join_result, 1, &value_buffer_name.to_string())
                    .unwrap();

                let value_buffer_pointer_name = scope.get_local_name();
                let value_buffer_pointer = if_some_builder
                    .build_alloca(
                        compiler.built_in_values._value_buffer_type,
                        &value_buffer_pointer_name.to_string(),
                    )
                    .unwrap();

                if_some_builder
                    .build_store(value_buffer_pointer, value_buffer)
                    .unwrap();

                let if_some_result_pointer_name = scope.get_local_name();
                let if_some_result_pointer = if_some_builder
                    .build_bit_cast(
                        value_buffer_pointer,
                        compiler.context.ptr_type(AddressSpace::default()),
                        &if_some_result_pointer_name.to_string(),
                    )
                    .unwrap()
                    .into_pointer_value();

                let if_some_result_name = scope.get_local_name();
                let if_some_result = if_some_builder
                    .build_load(
                        return_type,
                        if_some_result_pointer,
                        &if_some_result_name.to_string(),
                    )
                    .unwrap();

                UlarValue::from_basic_value(
                    compiler.context,
                    &call_type,
                    if_some_result,
                    self.call.get_position(),
                )?
            }

            None => UlarValue::Unit,
        };

        if_some_builder
            .build_unconditional_branch(end_block)
            .unwrap();

        let if_none_builder = compiler.context.create_builder();

        if_none_builder.position_at_end(if_none_block);

        let if_none_result = compiler.compile_inline_call(
            &if_none_builder,
            scope,
            self.function,
            &self.arguments,
            &self.call.get_type(),
            self.call.get_position(),
        )?;

        if_none_builder
            .build_unconditional_branch(end_block)
            .unwrap();
        builder.position_at_end(end_block);

        let result_name = scope.get_local_name();

        UlarValue::build_phi(
            compiler.context,
            builder,
            &call_type,
            &[
                (if_some_result, if_some_block),
                (if_none_result, if_none_block),
            ],
            result_name,
            self.call.get_position(),
        )
    }
}

#[derive(Clone, Copy)]
struct CompilableIf<'a>(&'a AnalyzedIf);

impl<'context> InlineExpression<'context> for CompilableIf<'_> {
    fn compile(
        &self,
        compiler: &mut JitFunctionCompiler<'_, 'context>,
        builder: &Builder<'context>,
        scope: &mut JitCompilerScope<'_, 'context>,
    ) -> Result<UlarValue<'context>, CompilationError> {
        let condition_name = scope.get_local_name();
        let condition = scope.get(
            &self.0.condition,
            condition_name,
            compiler.context,
            builder,
            compiler.built_in_values,
            compiler.execution_engine,
            compiler.module,
        )?;

        let then_block = compiler.append_basic_block(scope);
        let else_block = compiler.append_basic_block(scope);

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
        let then_builder = compiler.context.create_builder();

        then_builder.position_at_end(then_block);

        let then_value = compiler.compile_block(&then_builder, scope, &self.0.then_block)?;
        let else_builder = compiler.context.create_builder();

        else_builder.position_at_end(else_block);

        let else_value = compiler.compile_block(&else_builder, scope, &self.0.else_block)?;
        let end_block = compiler.append_basic_block(scope);

        then_builder.build_unconditional_branch(end_block).unwrap();
        else_builder.build_unconditional_branch(end_block).unwrap();
        builder.position_at_end(end_block);

        UlarValue::build_phi(
            compiler.context,
            builder,
            &self.0.type_,
            &[
                (then_value, then_builder.get_insert_block().unwrap()),
                (else_value, else_builder.get_insert_block().unwrap()),
            ],
            scope.get_local_name(),
            self.0.get_position(),
        )
    }
}

#[derive(Clone, Copy)]
struct CompilableInfixOperation<'a>(&'a AnalyzedInfixOperation);

impl<'context> InlineExpression<'context> for CompilableInfixOperation<'_> {
    fn compile(
        &self,
        compiler: &mut JitFunctionCompiler<'_, 'context>,
        builder: &Builder<'context>,
        scope: &mut JitCompilerScope<'_, 'context>,
    ) -> Result<UlarValue<'context>, CompilationError> {
        let left_name = scope.get_local_name();
        let left_value: IntValue = scope
            .get(
                &self.0.left,
                left_name,
                compiler.context,
                builder,
                compiler.built_in_values,
                compiler.execution_engine,
                compiler.module,
            )?
            .try_into()?;

        let right_name = scope.get_local_name();
        let right_value: IntValue = scope
            .get(
                &self.0.right,
                right_name,
                compiler.context,
                builder,
                compiler.built_in_values,
                compiler.execution_engine,
                compiler.module,
            )?
            .try_into()?;

        let name = scope.get_local_name();

        match self.0.operator {
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

            InfixOperator::Division => match self.0.get_type() {
                Type::Numeric(numeric_type) => {
                    let division_function = compiler
                        .built_in_values
                        .get_division_function_mut(numeric_type)
                        .get_inkwell_function(
                            compiler.context,
                            compiler.execution_engine,
                            compiler.module,
                        );

                    UlarValue::from_call_site_value(
                        compiler.context,
                        &Type::Numeric(numeric_type),
                        builder
                            .build_call(
                                division_function,
                                &[left_value.into(), right_value.into()],
                                &name.to_string(),
                            )
                            .unwrap(),
                        self.0.get_position(),
                    )
                }

                type_ => Err(CompilationError {
                    message: CompilationErrorMessage::InternalError(
                        InternalError::JitCompilerExpectedNumericType {
                            actual_type: format!("{}", type_),
                        },
                    ),

                    position: Some(self.0.get_position()),
                }),
            },

            InfixOperator::Modulo => match self.0.get_type() {
                Type::Numeric(numeric_type) => Ok(if numeric_type.is_signed() {
                    builder.build_int_signed_rem(left_value, right_value, &name.to_string())
                } else {
                    builder.build_int_unsigned_rem(left_value, right_value, &name.to_string())
                }
                .unwrap()
                .into()),

                type_ => Err(CompilationError {
                    message: CompilationErrorMessage::InternalError(
                        InternalError::JitCompilerExpectedNumericType {
                            actual_type: format!("{}", type_),
                        },
                    ),

                    position: Some(self.0.get_position()),
                }),
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
}

#[derive(Clone, Copy)]
struct CompilablePrefixOperation<'a>(&'a AnalyzedPrefixOperation);

impl<'context> InlineExpression<'context> for CompilablePrefixOperation<'_> {
    fn compile(
        &self,
        compiler: &mut JitFunctionCompiler<'_, 'context>,
        builder: &Builder<'context>,
        scope: &mut JitCompilerScope<'_, 'context>,
    ) -> Result<UlarValue<'context>, CompilationError> {
        let expression_name = scope.get_local_name();
        let expression = scope.get(
            &self.0.expression,
            expression_name,
            compiler.context,
            builder,
            compiler.built_in_values,
            compiler.execution_engine,
            compiler.module,
        )?;

        let name = scope.get_local_name();

        match self.0.operator {
            SimplePrefixOperator::Not => Ok(builder
                .build_xor(
                    TryInto::<IntValue>::try_into(expression)?,
                    compiler.context.i8_type().const_int(1, false),
                    &name.to_string(),
                )
                .unwrap()
                .into()),
        }
    }
}

struct JitFunctionCompiler<'a, 'context> {
    context: &'context Context,
    built_in_values: &'a mut BuiltInValues<'context>,
    fork_function_cache: &'a mut ForkFunctionCache<'context>,
    function: FunctionValue<'context>,
    execution_engine: &'a ExecutionEngine<'context>,
    module: &'a mut UlarModule<'context>,
}

impl<'context> JitFunctionCompiler<'_, 'context> {
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
        block: &AnalyzedBlock,
    ) -> Result<UlarValue<'context>, CompilationError> {
        scope.with_child_same_function(block.expression_graph.offset, |child_scope| {
            self.compile_expression_graph(builder, child_scope, &block.expression_graph)?;

            Ok(match &block.result {
                Some(result_reference) => {
                    let result_name = child_scope.get_local_name();

                    child_scope.get(
                        result_reference,
                        result_name,
                        self.context,
                        builder,
                        self.built_in_values,
                        self.execution_engine,
                        self.module,
                    )?
                }

                None => UlarValue::Unit,
            })
        })
    }

    fn compile_expression<'b>(
        expression: &'b AnalyzedExpression,
    ) -> Box<dyn CompilableExpression<'b, 'context> + 'b>
    where
        'context: 'b,
    {
        match expression {
            AnalyzedExpression::If(if_expression) => Box::new(CompilableIf(if_expression)),
            AnalyzedExpression::InfixOperation(infix_operation) => {
                Box::new(CompilableInfixOperation(infix_operation))
            }

            AnalyzedExpression::Call(call) => Box::new(CompilableCall(call)),
            AnalyzedExpression::PrefixOperation(prefix_operation) => {
                Box::new(CompilablePrefixOperation(prefix_operation))
            }
        }
    }

    fn compile_expression_graph(
        &mut self,
        builder: &Builder<'context>,
        scope: &mut JitCompilerScope<'_, 'context>,
        expression_graph: &DirectedGraph<AnalyzedExpression>,
    ) -> Result<(), CompilationError> {
        let compile_inline =
            |compiler: &mut Self, builder, scope: &mut JitCompilerScope<'_, 'context>, i| {
                let compilable = Self::compile_expression(expression_graph.get_node(i).unwrap());
                let value = compilable.compile_inline(compiler, builder, scope)?;

                scope.set_expression(i, value);

                Ok(())
            };

        let topological_sort = expression_graph.topological_sort();

        let mut forked_isolated_nodes = Vec::with_capacity(topological_sort.isolated_nodes.len());

        if topological_sort.layers.is_empty() {
            if let Some(&i) = topological_sort.isolated_nodes.last() {
                for &i in
                    &topological_sort.isolated_nodes[..topological_sort.isolated_nodes.len() - 1]
                {
                    let compilable =
                        Self::compile_expression(expression_graph.get_node(i).unwrap());

                    forked_isolated_nodes.push(compilable.compile_fork(self, builder, scope)?);
                }

                compile_inline(self, builder, scope, i)?;
            }
        } else {
            for &i in &topological_sort.isolated_nodes {
                let compilable = Self::compile_expression(expression_graph.get_node(i).unwrap());

                forked_isolated_nodes.push(compilable.compile_fork(self, builder, scope)?);
            }

            for layer in &topological_sort.layers {
                if let Some(&i) = layer.last() {
                    let mut forked = Vec::with_capacity(layer.len());

                    for &i in &layer[..layer.len() - 1] {
                        let compilable =
                            Self::compile_expression(expression_graph.get_node(i).unwrap());

                        forked.push(compilable.compile_fork(self, builder, scope)?);
                    }

                    compile_inline(self, builder, scope, i)?;

                    for (&i, forked) in layer.iter().zip(forked) {
                        let joined = forked.compile_join(self, builder, scope)?;

                        scope.set_expression(i, joined);
                    }
                }
            }
        }

        for (&i, forked) in topological_sort
            .isolated_nodes
            .iter()
            .zip(forked_isolated_nodes)
        {
            let joined = forked.compile_join(self, builder, scope)?;

            scope.set_expression(i, joined);
        }

        Ok(())
    }

    fn compile_function_definition(
        &mut self,
        builder: &Builder<'context>,
        scope: &mut JitCompilerScope<'_, 'context>,
        i: usize,
        definition: &AnalyzedFunctionDefinition,
    ) -> Result<(), CompilationError> {
        let local_name = scope.get_local_name();
        let ular_function = UlarFunction::try_from(scope.get(
            &AnalyzedExpressionRef::Function {
                index: i,
                type_: definition.get_type(),
                position: definition.get_position(),
            },
            local_name,
            self.context,
            builder,
            self.built_in_values,
            self.execution_engine,
            self.module,
        )?)?;

        let function = ular_function.try_into()?;
        let entry_block = self.context.append_basic_block(function, "entry");
        let worker_parameter = function.get_first_param().unwrap().into_pointer_value();
        let mut parameter_values = Vec::new();

        for (definition_parameter, function_parameter) in definition
            .parameters
            .iter()
            .zip(function.get_param_iter().skip(1))
        {
            parameter_values.push(UlarValue::from_basic_value(
                self.context,
                &definition_parameter.get_type(),
                function_parameter,
                definition_parameter.get_position(),
            )?);
        }

        let mut function_scope =
            JitCompilerScope::new_with_parent(scope, Some(&parameter_values), 0, worker_parameter);

        let mut function_compiler = JitFunctionCompiler {
            context: self.context,
            built_in_values: self.built_in_values,
            fork_function_cache: self.fork_function_cache,
            function,
            execution_engine: self.execution_engine,
            module: self.module,
        };

        /*
         * Before we compile the function, the position of `builder` is unknown. We need to compile it
         * with its own builder so we don't have to reset `builder`'s to its position before we begin
         * compiling the function, which is impossible insofar as I'm aware.
         */
        let function_builder = self.context.create_builder();

        function_builder.position_at_end(entry_block);
        function_compiler.compile_expression_graph(
            &function_builder,
            &mut function_scope,
            &definition.body.expression_graph,
        )?;

        let result_value = match &definition.body.result {
            Some(result_reference) => {
                let result_name = function_scope.get_local_name();

                BasicValueEnum::try_from(function_scope.get(
                    result_reference,
                    result_name,
                    self.context,
                    &function_builder,
                    self.built_in_values,
                    self.execution_engine,
                    self.module,
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

    /// Compiles a call without fork/join semantics.
    fn compile_inline_call(
        &mut self,
        builder: &Builder<'context>,
        scope: &mut JitCompilerScope<'_, 'context>,
        function: UlarFunction<'context>,
        arguments: &[UlarValue<'context>],
        type_: &Type,
        position: Position,
    ) -> Result<UlarValue<'context>, CompilationError> {
        builder
            .build_call(
                self.built_in_values._worker_tick.get_inkwell_function(
                    self.context,
                    self.execution_engine,
                    self.module,
                ),
                &[scope.worker.into()],
                "",
            )
            .unwrap();

        let mut argument_values = vec![scope.worker.into()];

        for &argument in arguments {
            argument_values.push(BasicValueEnum::try_from(argument)?.into());
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

        UlarValue::from_call_site_value(self.context, type_, call_result, position)
    }
}

fn compile_main_function<'a>(
    context: &'a Context,
    builder: &Builder<'a>,
    built_in_values: &mut BuiltInValues<'a>,
    fork_function_cache: &mut ForkFunctionCache<'a>,
    module: &mut UlarModule<'a>,
    execution_engine: &ExecutionEngine<'a>,
    program: &AnalyzedProgram,
) -> Result<FunctionValue<'a>, CompilationError> {
    let main_function = module.underlying.add_function(
        MAIN_FUNCTION_NAME,
        context
            .void_type()
            .fn_type(&[context.ptr_type(AddressSpace::default()).into()], false),
        None,
    );

    let worker = main_function
        .get_first_param()
        .unwrap()
        .into_pointer_value();

    let main_entry_block = context.append_basic_block(main_function, "entry");

    builder.position_at_end(main_entry_block);

    let mut function_values = Vec::new();

    for definition in &program.functions {
        let function_type =
            definition
                .type_
                .inkwell_type(context)
                .ok_or_else(|| CompilationError {
                    message: CompilationErrorMessage::UnitPassedAsValue,
                    position: Some(definition.get_position()),
                })?;

        let function = module
            .underlying
            .add_function(&definition.name.value, function_type, None);

        function_values.push(UlarValue::Function(UlarFunction::DirectReference(function)));
    }

    let mut scope = JitCompilerScope::new_without_parent(&function_values, None, worker);
    let mut function_compiler = JitFunctionCompiler {
        context,
        built_in_values,
        fork_function_cache,
        execution_engine,
        function: main_function,
        module,
    };

    for (i, definition) in program.functions.iter().enumerate() {
        function_compiler.compile_function_definition(builder, &mut scope, i, definition)?;
    }

    function_compiler.compile_expression_graph(builder, &mut scope, &program.expression_graph)?;

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
fn compile_main_harness_function<'a>(
    context: &'a Context,
    builder: &Builder<'a>,
    built_in_values: &mut BuiltInValues<'a>,
    module: &mut UlarModule<'a>,
    execution_engine: &ExecutionEngine<'a>,
    main_function: FunctionValue<'a>,
) {
    let main_harness_function = module.underlying.add_function(
        MAIN_HARNESS_FUNCTION_NAME,
        context.i8_type().fn_type(&[], false),
        None,
    );

    let entry_block = context.append_basic_block(main_harness_function, "entry");
    let catch_block = context.append_basic_block(main_harness_function, "catch");
    let end_block = context.append_basic_block(main_harness_function, "end");

    builder.position_at_end(entry_block);

    let worker_pool = builder
        .build_call(
            built_in_values
                ._workerpool_new
                .get_inkwell_function(context, execution_engine, module),
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
                context,
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
        .build_invoke(main_function, &[worker], end_block, catch_block, "")
        .unwrap();

    builder.position_at_end(catch_block);

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

    builder.build_unconditional_branch(end_block).unwrap();
    builder.position_at_end(end_block);

    let phi = builder
        .build_phi(context.i8_type(), "return_value")
        .unwrap();

    phi.add_incoming(&[
        (&context.i8_type().const_int(0, false), entry_block),
        (&context.i8_type().const_int(1, false), catch_block),
    ]);

    builder
        .build_call(
            built_in_values
                ._worker_free
                .get_inkwell_function(context, execution_engine, module),
            &[worker.into()],
            "",
        )
        .unwrap();

    builder
        .build_call(
            built_in_values._workerpool_join.get_inkwell_function(
                context,
                execution_engine,
                module,
            ),
            &[worker_pool],
            "",
        )
        .unwrap();

    builder.build_return(Some(&phi.as_basic_value())).unwrap();
}

fn compile_program<'a>(
    context: &'a Context,
    built_in_values: &mut BuiltInValues<'a>,
    fork_function_cache: &mut ForkFunctionCache<'a>,
    module: &mut UlarModule<'a>,
    execution_engine: &ExecutionEngine<'a>,
    program: &AnalyzedProgram,
    print_to_stderr: bool,
) -> Result<JitFunction<'a, MainFunction>, CompilationError> {
    let builder = context.create_builder();
    let main_function = compile_main_function(
        context,
        &builder,
        built_in_values,
        fork_function_cache,
        module,
        execution_engine,
        program,
    )?;

    compile_main_harness_function(
        context,
        &builder,
        built_in_values,
        module,
        execution_engine,
        main_function,
    );

    if print_to_stderr {
        debug!("Output of the jit_compiler phase:");
        debug!("{}", module.underlying.print_to_string().to_string());
    }

    Ok(unsafe {
        execution_engine
            .get_function(MAIN_HARNESS_FUNCTION_NAME)
            .unwrap()
    })
}

pub fn compile_and_execute_program(
    program: &AnalyzedProgram,
    print_to_stderr: bool,
) -> Result<u8, CompilationError> {
    let context = Context::create();
    let mut built_in_values = BuiltInValues::new(&context);
    let mut fork_function_cache = ForkFunctionCache::new();
    let mut module: UlarModule = context.create_module("main").into();
    let execution_engine = module
        .underlying
        .create_jit_execution_engine(OptimizationLevel::None)
        .unwrap();

    let main_function = compile_program(
        &context,
        &mut built_in_values,
        &mut fork_function_cache,
        &mut module,
        &execution_engine,
        program,
        print_to_stderr,
    )?;

    Ok(unsafe { main_function.call() })
}

fn get_value_buffer_type(context: &Context) -> ArrayType {
    context.i64_type().array_type(VALUE_BUFFER_WORD_SIZE as u32)
}
