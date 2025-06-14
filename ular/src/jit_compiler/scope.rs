use crate::{
    data_structures::number_map::NumberMap,
    dependency_analyzer::analyzed_program::AnalyzedExpressionRef,
    error_reporting::{CompilationError, CompilationErrorMessage, InternalError},
    jit_compiler::{built_in_values::BuiltInValues, module::UlarModule, value::UlarValue},
};
use inkwell::{
    builder::Builder, context::Context, execution_engine::ExecutionEngine, values::PointerValue,
};
use std::{
    cmp::Eq,
    fmt::{Display, Formatter},
};

pub struct JitCompilerScope<'a, 'context> {
    parent: Option<&'a JitCompilerScope<'a, 'context>>,
    function_values: &'a [UlarValue<'context>],
    parameter_values: Option<&'a [UlarValue<'context>]>,
    next_local_name: LocalName,
    expression_values: NumberMap<UlarValue<'context>>,
    pub worker: PointerValue<'context>,
}

impl<'a, 'context> JitCompilerScope<'a, 'context> {
    pub fn get_local_name(&mut self) -> LocalName {
        let result = self.next_local_name;

        self.next_local_name.0 += 1;

        result
    }

    #[allow(clippy::too_many_arguments)]
    pub fn get(
        &self,
        reference: &AnalyzedExpressionRef,
        local_name: LocalName,
        context: &'context Context,
        builder: &Builder<'context>,
        built_in_values: &mut BuiltInValues<'context>,
        execution_engine: &ExecutionEngine<'context>,
        module: &mut UlarModule<'context>,
    ) -> Result<UlarValue<'context>, CompilationError> {
        match reference {
            AnalyzedExpressionRef::BuiltIn { name, .. } => built_in_values
                .get(name, local_name, context, builder, execution_engine, module)
                .ok_or_else(|| InternalError::UnknownValue { name: name.clone() }),

            AnalyzedExpressionRef::Expression { index, .. } => self
                .get_expression(*index)
                .ok_or(InternalError::JitCompilerUnknownExpression { index: *index }),

            AnalyzedExpressionRef::Function { index, .. } => match self.function_values.get(*index)
            {
                Some(value) => Ok(*value),
                None => Err(InternalError::JitCompilerUnknownFunction { index: *index }),
            },

            AnalyzedExpressionRef::Parameter { index, .. } => self
                .parameter_values
                .and_then(|parameter_values| parameter_values.get(*index).copied())
                .ok_or(InternalError::JitCompilerUnknownParameter { index: *index }),

            AnalyzedExpressionRef::Number(number) => Ok(UlarValue::Int(
                number
                    .type_
                    .inkwell_type(context)
                    .const_int(number.value as u64, number.type_.is_signed()),
            )),

            AnalyzedExpressionRef::Unit(_) => Ok(UlarValue::Unit),
        }
        .map_err(|internal_error| CompilationError {
            message: CompilationErrorMessage::InternalError(internal_error),
            position: None,
        })
    }

    fn get_expression(&self, i: usize) -> Option<UlarValue<'context>> {
        self.expression_values
            .get(i)
            .copied()
            .or_else(|| self.parent.and_then(|parent| parent.get_expression(i)))
    }

    pub fn set_expression(&mut self, i: usize, value: UlarValue<'context>) {
        self.expression_values.insert(i, value);
    }

    pub fn with_child_same_function<A: FnOnce(&mut JitCompilerScope<'_, 'context>) -> B, B>(
        &mut self,
        offset: usize,
        callback: A,
    ) -> B {
        let mut child =
            JitCompilerScope::new_with_parent(self, self.parameter_values, offset, self.worker);

        let result = callback(&mut child);

        self.next_local_name = child.next_local_name;

        result
    }

    pub fn new_with_parent(
        parent: &'a JitCompilerScope<'a, 'context>,
        parameter_values: Option<&'a [UlarValue<'context>]>,
        offset: usize,
        worker: PointerValue<'context>,
    ) -> Self {
        Self {
            parent: Some(parent),
            function_values: parent.function_values,
            parameter_values,
            next_local_name: parent.next_local_name,
            expression_values: NumberMap::new(offset),
            worker,
        }
    }

    pub fn new_without_parent(
        function_values: &'a [UlarValue<'context>],
        parameter_values: Option<&'a [UlarValue<'context>]>,
        worker: PointerValue<'context>,
    ) -> Self {
        Self {
            parent: None,
            function_values,
            parameter_values,
            next_local_name: LocalName(0),
            expression_values: NumberMap::new(0),
            worker,
        }
    }
}

#[derive(Clone, Copy, Eq, Hash, PartialEq)]
pub struct LocalName(u32);

impl Display for LocalName {
    fn fmt(&self, formatter: &mut Formatter<'_>) -> std::fmt::Result {
        write!(formatter, "{}", self.0)
    }
}
