use crate::{
    data_structures::{graph::DirectedGraph, number_map::NumberMap, IteratorExtension},
    dependency_analyzer::analyzed_program::{
        AnalyzedBlock, AnalyzedCall, AnalyzedExpression, AnalyzedFunctionDefinition, AnalyzedIf,
        AnalyzedInfixOperation, AnalyzedPrefixOperation, AnalyzedSelect, AnalyzedStringLiteral,
        AnalyzedStructApplication, AnalyzedType, AnalyzerTyped,
    },
    error_reporting::{CompilationError, CompilationErrorMessage, InternalError, Position},
    jit_compiler::{
        module::{
            built_in_values::{BuiltInFunction, JitCompilerBuiltInValues},
            fork_function_cache::ForkFunctionCache,
            StructInformation,
        },
        scope::{JitCompilerScope, JitCompilerScopeContext},
        value::{UlarFunction, UlarValue},
    },
    parser::program::{
        InfixOperator, LogicalInfixOperator, Node, NumericInfixOperator, UniversalInfixOperator,
    },
    simplifier::simple_program::SimplePrefixOperator,
};
use inkwell::{
    basic_block::BasicBlock,
    builder::Builder,
    context::Context,
    execution_engine::ExecutionEngine,
    values::{BasicValueEnum, FunctionValue, IntValue, PointerValue},
    AddressSpace, IntPredicate,
};
use std::collections::HashMap;

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

    fn is_forkable(&self) -> bool;
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

    fn is_forkable(&self) -> bool {
        false
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
        builder: &Builder<'context>,
        scope: &mut JitCompilerScope<'_, 'context>,
    ) -> Result<(UlarFunction<'context>, Vec<UlarValue<'context>>), CompilationError> {
        let function_name = scope.get_local_name();
        let function: UlarFunction = scope
            .get(&self.0.function, function_name, builder)?
            .try_into()?;

        let mut arguments = Vec::with_capacity(self.0.arguments.len());

        for argument_reference in &self.0.arguments {
            let name = scope.get_local_name();

            arguments.push(scope.get(argument_reference, name, builder)?);
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
        let (function, arguments) = self.get_function_and_arguments(builder, scope)?;
        let fork_function = compiler.fork_function_cache.get_or_build(function);
        let fork_function_pointer = fork_function
            .function()
            .as_global_value()
            .as_pointer_value();

        let context = fork_function.build_context(builder, scope, &arguments)?;

        let job_pointer_name = scope.get_local_name();
        let job_pointer = builder
            .build_alloca(
                compiler.built_in_values._job_type,
                &job_pointer_name.to_string(),
            )
            .unwrap();

        builder
            .build_call(
                compiler
                    .built_in_values
                    ._job_new
                    .get_inkwell_function(compiler.built_in_values),
                &[job_pointer.into()],
                "",
            )
            .unwrap();

        builder
            .build_call(
                compiler
                    .built_in_values
                    ._worker_fork
                    .get_inkwell_function(compiler.built_in_values),
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
        let (function, arguments) = self.get_function_and_arguments(builder, scope)?;

        compiler.compile_inline_call(
            builder,
            scope,
            function,
            &arguments,
            &self.0.get_type(),
            self.0.get_position(),
        )
    }

    fn is_forkable(&self) -> bool {
        true
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
                    .get_inkwell_function(compiler.built_in_values),
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
        let condition = scope.get(&self.0.condition, condition_name, builder)?;
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

impl<'context> CompilableInfixOperation<'_> {
    fn compile_logical_operation(
        builder: &Builder<'context>,
        scope: &mut JitCompilerScope<'_, 'context>,
        left_value: UlarValue<'context>,
        right_value: UlarValue<'context>,
        operator: LogicalInfixOperator,
    ) -> Result<UlarValue<'context>, CompilationError> {
        let left_int_value = IntValue::try_from(left_value)?;
        let right_int_value = IntValue::try_from(right_value)?;
        let name = scope.get_local_name();

        match operator {
            LogicalInfixOperator::LogicalAnd => Ok(builder
                .build_and(left_int_value, right_int_value, &name.to_string())
                .unwrap()
                .into()),

            LogicalInfixOperator::LogicalOr => Ok(builder
                .build_or(left_int_value, right_int_value, &name.to_string())
                .unwrap()
                .into()),
        }
    }

    fn compile_numeric_operation(
        &self,
        compiler: &mut JitFunctionCompiler<'_, 'context>,
        builder: &Builder<'context>,
        scope: &mut JitCompilerScope<'_, 'context>,
        left_value: UlarValue<'context>,
        right_value: UlarValue<'context>,
        operator: NumericInfixOperator,
    ) -> Result<UlarValue<'context>, CompilationError> {
        let left_int_value = IntValue::try_from(left_value)?;
        let right_int_value = IntValue::try_from(right_value)?;
        let argument_numeric_type = match self.0.left.get_type() {
            AnalyzedType::Numeric(numeric_type) => numeric_type,
            type_ => {
                return Err(CompilationError {
                    message: CompilationErrorMessage::InternalError(
                        InternalError::JitCompilerExpectedNumericType {
                            actual_type: format!(
                                "{}",
                                type_.display(|i| compiler.struct_information[i].definition)
                            ),
                        },
                    ),

                    position: Some(self.0.get_position()),
                })
            }
        };

        let name = scope.get_local_name();

        match operator {
            NumericInfixOperator::Addition => Ok(builder
                .build_int_add(left_int_value, right_int_value, &name.to_string())
                .unwrap()
                .into()),

            NumericInfixOperator::Subtraction => Ok(builder
                .build_int_sub(left_int_value, right_int_value, &name.to_string())
                .unwrap()
                .into()),

            NumericInfixOperator::Multiplication => Ok(builder
                .build_int_mul(left_int_value, right_int_value, &name.to_string())
                .unwrap()
                .into()),

            NumericInfixOperator::Division => {
                let division_function = compiler
                    .built_in_values
                    .get_division_function(argument_numeric_type)
                    .get_inkwell_function(compiler.built_in_values);

                UlarValue::from_call_site_value(
                    compiler.context,
                    &AnalyzedType::Numeric(argument_numeric_type),
                    builder
                        .build_call(
                            division_function,
                            &[left_value.try_into()?, right_value.try_into()?],
                            &name.to_string(),
                        )
                        .unwrap(),
                    self.0.get_position(),
                )
            }

            NumericInfixOperator::Modulo => Ok(if argument_numeric_type.is_signed() {
                builder.build_int_signed_rem(left_int_value, right_int_value, &name.to_string())
            } else {
                builder.build_int_unsigned_rem(left_int_value, right_int_value, &name.to_string())
            }
            .unwrap()
            .into()),

            operator => Ok(builder
                .build_int_compare(
                    match (operator, argument_numeric_type.is_signed()) {
                        (NumericInfixOperator::LessThan, false) => IntPredicate::ULT,
                        (NumericInfixOperator::LessThan, true) => IntPredicate::SLT,
                        (NumericInfixOperator::LessThanOrEqual, false) => IntPredicate::ULE,
                        (NumericInfixOperator::LessThanOrEqual, true) => IntPredicate::SLE,
                        (NumericInfixOperator::GreaterThan, false) => IntPredicate::UGT,
                        (NumericInfixOperator::GreaterThan, true) => IntPredicate::SGT,
                        (NumericInfixOperator::GreaterThanOrEqual, false) => IntPredicate::UGE,
                        (NumericInfixOperator::GreaterThanOrEqual, true) => IntPredicate::SGE,
                        _ => panic!("Unexpected comparison operator: {:?}", operator),
                    },
                    left_int_value,
                    right_int_value,
                    &name.to_string(),
                )
                .unwrap()
                .into()),
        }
    }

    fn compile_universal_comparison(
        compiler: &mut JitFunctionCompiler<'_, 'context>,
        builder: &Builder<'context>,
        scope: &mut JitCompilerScope<'_, 'context>,
        left_value: UlarValue<'context>,
        right_value: UlarValue<'context>,
        operator: UniversalInfixOperator,
        position: Position,
    ) -> Result<UlarValue<'context>, CompilationError> {
        let name = scope.get_local_name();

        match left_value {
            UlarValue::Function(left_function) => {
                let left_pointer = left_function.get_pointer_value();
                let right_pointer = UlarFunction::try_from(right_value)?.get_pointer_value();

                Ok(builder
                    .build_int_compare(
                        match operator {
                            UniversalInfixOperator::EqualComparison => IntPredicate::EQ,
                            UniversalInfixOperator::UnequalComparison => IntPredicate::NE,
                        },
                        left_pointer,
                        right_pointer,
                        &name.to_string(),
                    )
                    .unwrap()
                    .into())
            }

            UlarValue::Int(left_int) => {
                let right_int = right_value.try_into()?;

                Ok(builder
                    .build_int_compare(
                        match operator {
                            UniversalInfixOperator::EqualComparison => IntPredicate::EQ,
                            UniversalInfixOperator::UnequalComparison => IntPredicate::NE,
                        },
                        left_int,
                        right_int,
                        &name.to_string(),
                    )
                    .unwrap()
                    .into())
            }

            UlarValue::String(_) => Err(CompilationError {
                message: CompilationErrorMessage::InternalError(
                    InternalError::JitCompilerStringComparisonNotRewritten,
                ),

                position: Some(position),
            }),

            UlarValue::Struct(_) => Err(CompilationError {
                message: CompilationErrorMessage::InternalError(
                    InternalError::JitCompilerStructComparisonNotRewritten,
                ),

                position: Some(position),
            }),

            UlarValue::Unit => Ok(compiler.context.i8_type().const_int(1, false).into()),
        }
    }
}

impl<'context> InlineExpression<'context> for CompilableInfixOperation<'_> {
    fn compile(
        &self,
        compiler: &mut JitFunctionCompiler<'_, 'context>,
        builder: &Builder<'context>,
        scope: &mut JitCompilerScope<'_, 'context>,
    ) -> Result<UlarValue<'context>, CompilationError> {
        let left_name = scope.get_local_name();
        let left_value = scope.get(&self.0.left, left_name, builder)?;
        let right_name = scope.get_local_name();
        let right_value = scope.get(&self.0.right, right_name, builder)?;

        match self.0.operator {
            InfixOperator::Logical(operator) => {
                Self::compile_logical_operation(builder, scope, left_value, right_value, operator)
            }

            InfixOperator::Numeric(operator) => self.compile_numeric_operation(
                compiler,
                builder,
                scope,
                left_value,
                right_value,
                operator,
            ),

            InfixOperator::Universal(operator) => Self::compile_universal_comparison(
                compiler,
                builder,
                scope,
                left_value,
                right_value,
                operator,
                self.0.get_position(),
            ),
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
        let expression = scope.get(&self.0.expression, expression_name, builder)?;
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

#[derive(Clone, Copy)]
struct CompilableSelect<'a>(&'a AnalyzedSelect);

impl<'context> InlineExpression<'context> for CompilableSelect<'_> {
    fn compile(
        &self,
        compiler: &mut JitFunctionCompiler<'_, 'context>,
        builder: &Builder<'context>,
        scope: &mut JitCompilerScope<'_, 'context>,
    ) -> Result<UlarValue<'context>, CompilationError> {
        let struct_index = match self.0.left_hand_side.get_type() {
            AnalyzedType::Struct(i) => i,
            type_ => {
                return Err(CompilationError {
                    message: CompilationErrorMessage::InternalError(
                        InternalError::JitCompilerExpectedStructType {
                            actual_type: format!(
                                "{}",
                                type_.display(|i| compiler.struct_information[i].definition)
                            ),
                        },
                    ),

                    position: Some(self.0.get_position()),
                })
            }
        };

        let struct_information = &compiler.struct_information[struct_index];
        let struct_value_name = scope.get_local_name();
        let struct_value = scope.get(&self.0.left_hand_side, struct_value_name, builder)?;
        let field_pointer_name = scope.get_local_name();
        let field_pointer = builder
            .build_struct_gep(
                struct_information.inkwell_type,
                PointerValue::try_from(struct_value)?,
                // Add 1 to offset for the object descriptor reference
                self.0.field_index as u32 + 1,
                &field_pointer_name.to_string(),
            )
            .unwrap();

        let field_name = scope.get_local_name();
        let field_type = &struct_information.definition.fields[self.0.field_index].type_;
        let field = builder
            .build_load(
                field_type
                    .inkwell_type(compiler.context)
                    .ok_or_else(|| CompilationError {
                        message: CompilationErrorMessage::UnitPassedAsValue,
                        position: Some(self.0.get_position()),
                    })?,
                field_pointer,
                &field_name.to_string(),
            )
            .unwrap();

        UlarValue::from_basic_value(compiler.context, field_type, field, self.0.get_position())
    }
}

#[derive(Clone, Copy)]
struct CompilableStringLiteral<'a>(&'a AnalyzedStringLiteral);

impl<'context> InlineExpression<'context> for CompilableStringLiteral<'_> {
    fn compile(
        &self,
        compiler: &mut JitFunctionCompiler<'_, 'context>,
        _builder: &Builder<'context>,
        _scope: &mut JitCompilerScope<'_, 'context>,
    ) -> Result<UlarValue<'context>, CompilationError> {
        let i = self.0.index;

        compiler
            .scope_context
            .string_values
            .get(i)
            .copied()
            .ok_or(CompilationError {
                message: CompilationErrorMessage::InternalError(
                    InternalError::JitCompilerUnknownString { index: i },
                ),

                position: None,
            })
    }
}

#[derive(Clone, Copy)]
struct CompilableStructApplication<'a>(&'a AnalyzedStructApplication);

impl<'context> InlineExpression<'context> for CompilableStructApplication<'_> {
    fn compile(
        &self,
        compiler: &mut JitFunctionCompiler<'_, 'context>,
        builder: &Builder<'context>,
        scope: &mut JitCompilerScope<'_, 'context>,
    ) -> Result<UlarValue<'context>, CompilationError> {
        let struct_information = &compiler.struct_information[self.0.struct_index];
        let allocated_size = struct_information.inkwell_type.size_of().unwrap();
        let allocated_align = struct_information.inkwell_type.get_alignment();
        let struct_pointer_name = scope.get_local_name();
        let struct_pointer = builder
            .build_direct_call(
                compiler
                    .built_in_values
                    ._mmtk_alloc
                    .get_inkwell_function(compiler.built_in_values),
                &[allocated_size.into(), allocated_align.into()],
                &struct_pointer_name.to_string(),
            )
            .unwrap()
            .try_as_basic_value()
            .unwrap_left()
            .into_pointer_value();

        let field_expressions = self
            .0
            .fields
            .iter()
            .map(|field| {
                let field_value_name = scope.get_local_name();

                Ok((
                    field.name.clone(),
                    scope.get(&field.value, field_value_name, builder)?,
                ))
            })
            .collect::<Result<HashMap<_, _>, _>>()?;

        let descriptor_reference_field_name = scope.get_local_name();
        let descriptor_reference_field = unsafe {
            builder.build_in_bounds_gep(
                struct_information.inkwell_type,
                struct_pointer,
                &[
                    compiler.context.i32_type().const_int(0, false),
                    compiler.context.i32_type().const_int(0, false),
                ],
                &descriptor_reference_field_name.to_string(),
            )
        }
        .unwrap();

        builder
            .build_store(
                descriptor_reference_field,
                compiler
                    .context
                    .i32_type()
                    .const_int(struct_information.descriptor_reference.0.into(), false),
            )
            .unwrap();

        for (i, field) in struct_information.definition.fields.iter().enumerate() {
            let field_pointer_name = scope.get_local_name();

            // SAFETY: `i` is a valid index of the struct we're building
            let field_pointer = unsafe {
                builder.build_in_bounds_gep(
                    struct_information.inkwell_type,
                    struct_pointer,
                    &[
                        compiler.context.i32_type().const_int(0, false),
                        // Add 1 to offset for the object descriptor reference
                        compiler.context.i32_type().const_int(i as u64 + 1, false),
                    ],
                    &field_pointer_name.to_string(),
                )
            }
            .unwrap();

            builder
                .build_store::<BasicValueEnum>(
                    field_pointer,
                    field_expressions[&field.name.value].try_into()?,
                )
                .unwrap();
        }

        Ok(UlarValue::Struct(struct_pointer))
    }
}

pub(super) struct JitFunctionCompiler<'a, 'context> {
    pub(super) built_in_values: &'a JitCompilerBuiltInValues<'a, 'context>,
    pub(super) context: &'context Context,
    pub(super) execution_engine: &'a ExecutionEngine<'context>,
    pub(super) fork_function_cache: &'a ForkFunctionCache<'a, 'context>,
    pub(super) scope_context: &'a JitCompilerScopeContext<'context>,
    pub(super) struct_information: &'a [StructInformation<'a, 'context>],
    pub(super) function: FunctionValue<'context>,
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
        scope.with_child_same_function(block.expression_graph.get_offset(), |child_scope| {
            self.compile_expression_graph(builder, child_scope, &block.expression_graph)?;

            Ok(match &block.result {
                Some(result_reference) => {
                    let result_name = child_scope.get_local_name();

                    child_scope.get(result_reference, result_name, builder)?
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

            AnalyzedExpression::Select(select) => Box::new(CompilableSelect(select)),
            AnalyzedExpression::Call(call) => Box::new(CompilableCall(call)),
            AnalyzedExpression::StructApplication(struct_application) => {
                Box::new(CompilableStructApplication(struct_application))
            }

            AnalyzedExpression::PrefixOperation(prefix_operation) => {
                Box::new(CompilablePrefixOperation(prefix_operation))
            }

            AnalyzedExpression::String(string_literal) => {
                Box::new(CompilableStringLiteral(string_literal))
            }
        }
    }

    pub(super) fn compile_expression_graph(
        &mut self,
        builder: &Builder<'context>,
        scope: &mut JitCompilerScope<'_, 'context>,
        expression_graph: &DirectedGraph<AnalyzedExpression>,
    ) -> Result<(), CompilationError> {
        let mut compilables = NumberMap::new(expression_graph.get_offset());

        compilables.extend(
            expression_graph
                .node_iter()
                .map(|(i, node)| (i, Self::compile_expression(node))),
        );

        // Compiles an individual node as "inline", avoiding any fork/join operations.
        //
        // This is done if (1) the operation is so cheap that it's not worth forking (i.e.
        // `is_forkable` returned false) or (2) we're compiling an operation that we want to execute
        // while other threads are potentially busy executing forked operations.
        let compile_inline = |compiler: &mut Self,
                              builder: &Builder<'context>,
                              scope: &mut JitCompilerScope<'_, 'context>,
                              i: usize| {
            let compilable = compilables.get(i).unwrap();
            let value = compilable.compile_inline(compiler, builder, scope)?;

            scope.set_expression(i, value);

            Ok(())
        };

        // Compiles a layer within the topological sort.
        let compile_layer = |compiler: &mut Self,
                             builder: &Builder<'context>,
                             scope: &mut JitCompilerScope<'_, 'context>,
                             layer: &[usize]| {
            let multiple_are_forkable = compilables
                .values()
                .at_least(|compilable| compilable.is_forkable(), 2);

            if !multiple_are_forkable {
                // If fewer than two operations in the layer are expensive enough to warrant
                // fork/join-ing, then compile all the operations as "inline" to avoid
                // unnecessary overhead.
                for &i in layer.iter() {
                    compile_inline(compiler, builder, scope, i)?;
                }
            } else {
                // Otherwise, fork all operations but the last one, execute the last one in the
                // current thread, and then join the operations we forked.
                let mut forked = Vec::with_capacity(layer.len() - 1);

                for &i in &layer[..layer.len() - 1] {
                    let compilable = compilables.get(i).unwrap();

                    forked.push(compilable.compile_fork(compiler, builder, scope)?);
                }

                compile_inline(compiler, builder, scope, layer[layer.len() - 1])?;

                for (&i, forked) in layer.iter().zip(forked) {
                    let joined = forked.compile_join(compiler, builder, scope)?;

                    scope.set_expression(i, joined);
                }
            }

            Ok(())
        };

        let topological_sort = expression_graph.topological_sort();
        let has_forkable_layers = topological_sort.layers.iter().any(|layer| {
            layer
                .iter()
                .any(|&i| compilables.get(i).unwrap().is_forkable())
        });

        let mut forked_isolated_nodes = Vec::with_capacity(topological_sort.isolated_nodes.len());

        if !has_forkable_layers {
            // If none of the layers have operations worth forking, then just compile the list of
            // isolated nodes as a layer. There's no point in forking all of them and then joining all
            // of them because we're not going to do any expensive work in between.
            compile_layer(self, builder, scope, &topological_sort.isolated_nodes)?;
        } else {
            // Fork all of the isolated nodes. After we're done, we'll compute all of the layers.
            // Then, we'll join all of the isolated nodes.
            for &i in &topological_sort.isolated_nodes {
                let compilable = Self::compile_expression(expression_graph.get_node(i).unwrap());

                forked_isolated_nodes.push(compilable.compile_fork(self, builder, scope)?);
            }
        }

        for layer in &topological_sort.layers {
            compile_layer(self, builder, scope, layer)?;
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

    pub(super) fn compile_function_definition(
        &mut self,
        scope: &mut JitCompilerScope<'_, 'context>,
        definition: &AnalyzedFunctionDefinition,
        function: FunctionValue<'context>,
    ) -> Result<(), CompilationError> {
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
                &definition_parameter.type_,
                function_parameter,
                definition_parameter.get_position(),
            )?);
        }

        let mut function_scope =
            JitCompilerScope::new_with_parent(scope, Some(&parameter_values), 0, worker_parameter);

        let mut function_compiler = JitFunctionCompiler {
            built_in_values: self.built_in_values,
            context: self.context,
            execution_engine: self.execution_engine,
            fork_function_cache: self.fork_function_cache,
            scope_context: self.scope_context,
            struct_information: self.struct_information,
            function,
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
                    &function_builder,
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
        type_: &AnalyzedType,
        position: Position,
    ) -> Result<UlarValue<'context>, CompilationError> {
        builder
            .build_call(
                self.built_in_values
                    ._worker_tick
                    .get_inkwell_function(self.built_in_values),
                &[scope.worker.into()],
                "",
            )
            .unwrap();

        let mut argument_values = vec![scope.worker.into()];

        for &argument in arguments {
            argument_values.push(argument.try_into()?);
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
