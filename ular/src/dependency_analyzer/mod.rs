pub mod analyzed_program;
mod scope;

use crate::{
    data_structures::{graph::DirectedGraph, number_map::NumberMap},
    dependency_analyzer::{
        analyzed_program::{
            AnalyzedBlock, AnalyzedCall, AnalyzedExpression, AnalyzedExpressionRef,
            AnalyzedFunctionDefinition, AnalyzedIf, AnalyzedInfixOperation,
            AnalyzedPrefixOperation, AnalyzedProgram,
        },
        scope::AnalyzerScope,
    },
    error_reporting::{CompilationError, CompilationErrorMessage, InternalError},
    parser::program::Node,
    phase::Phase,
    typechecker::{
        built_in_values::BuiltInValues,
        typed_program::{
            Typed, TypedBlock, TypedCall, TypedExpression, TypedFunctionDefinition,
            TypedIdentifier, TypedIf, TypedInfixOperation, TypedPrefixOperation, TypedProgram,
            TypedStatement,
        },
    },
};

struct Analyzer<'a> {
    scope: AnalyzerScope<'a>,
    functions: &'a mut AnalyzerFunctions,
    expression_graph: AnalyzerExpressionGraph<'a>,
}

impl<'a> Analyzer<'a> {
    fn analyze_block(
        &mut self,
        block: &TypedBlock,
        surrounding_expression: usize,
    ) -> Result<AnalyzedBlock, CompilationError> {
        let mut analyzer = Analyzer::with_parent(self, Some(surrounding_expression));

        analyzer.analyze_statements_with_functions_hoisted(&block.statements)?;

        let result_reference = match &block.result {
            Some(result) => Some(analyzer.analyze_expression(result)?),
            None => None,
        };

        Ok(AnalyzedBlock {
            expression_graph: analyzer.into_expression_graph(),
            result: result_reference,
        })
    }

    fn analyze_call(
        &mut self,
        call: &TypedCall,
    ) -> Result<AnalyzedExpressionRef, CompilationError> {
        let function_reference = self.analyze_expression(&call.function)?;
        let mut argument_references = Vec::with_capacity(call.arguments.len());

        for argument in &call.arguments {
            argument_references.push(self.analyze_expression(argument)?);
        }

        let i = self
            .expression_graph
            .add_node(AnalyzedExpression::Call(AnalyzedCall {
                function: function_reference.clone(),
                arguments: argument_references.clone(),
                type_: call.get_type(),
                position: call.get_position(),
            }));

        self.expression_graph
            .add_edge_with_reference(i, &function_reference);

        for reference in &argument_references {
            self.expression_graph.add_edge_with_reference(i, reference);
        }

        Ok(AnalyzedExpressionRef::for_expression(i, call))
    }

    fn analyze_expression(
        &mut self,
        expression: &TypedExpression,
    ) -> Result<AnalyzedExpressionRef, CompilationError> {
        match expression {
            TypedExpression::If(if_expression) => self.analyze_if(if_expression),
            TypedExpression::InfixOperation(infix_operation) => {
                self.analyze_infix_operation(infix_operation)
            }

            TypedExpression::Call(call) => self.analyze_call(call),
            TypedExpression::Identifier(identifier) => self.analyze_identifier(identifier),
            TypedExpression::Number(number) => Ok(AnalyzedExpressionRef::Number(number.clone())),
            TypedExpression::PrefixOperation(prefix_operation) => {
                self.analyze_prefix_operation(prefix_operation)
            }
        }
    }

    fn analyze_function_definition<'b>(
        &'b mut self,
        i: usize,
        definition: &TypedFunctionDefinition,
    ) -> Result<(), CompilationError> {
        let mut analyzer = Analyzer::with_parent(self, None);

        for (j, parameter) in definition.parameters.iter().enumerate() {
            analyzer.scope.declare_variable(
                &parameter.underlying,
                AnalyzedExpressionRef::Parameter {
                    index: j,
                    type_: parameter.get_type(),
                    position: parameter.get_position(),
                },
            )?;
        }

        analyzer.analyze_statements_with_functions_hoisted(&definition.body.statements)?;

        let result_reference = match &definition.body.result {
            Some(result) => Some(analyzer.analyze_expression(result)?),
            None => None,
        };

        let analyzed_body = AnalyzedBlock {
            expression_graph: analyzer.into_expression_graph(),
            result: result_reference,
        };

        self.functions.set_function(
            i,
            AnalyzedFunctionDefinition {
                name: definition.name.clone(),
                parameters: definition.parameters.clone(),
                body: analyzed_body,
                type_: definition.type_.clone(),
                position: definition.get_position(),
            },
        );

        Ok(())
    }

    fn analyze_identifier(
        &self,
        identifier: &TypedIdentifier,
    ) -> Result<AnalyzedExpressionRef, CompilationError> {
        self.scope
            .get_variable(&identifier)
            .ok_or_else(|| CompilationError {
                message: CompilationErrorMessage::InternalError(InternalError::UnknownValue {
                    name: identifier.underlying.value.clone(),
                }),

                position: Some(identifier.get_position()),
            })
    }

    fn analyze_if(
        &mut self,
        if_expression: &TypedIf,
    ) -> Result<AnalyzedExpressionRef, CompilationError> {
        let condition_reference = self.analyze_expression(&if_expression.condition)?;
        let i = self.expression_graph.reserve_node();

        self.expression_graph
            .add_edge_with_reference(i, &condition_reference);

        let analyzed_then_block = self.analyze_block(&if_expression.then_block, i)?;
        let analyzed_else_block = self.analyze_block(&if_expression.else_block, i)?;

        self.expression_graph.set_node(
            i,
            AnalyzedExpression::If(AnalyzedIf {
                condition: condition_reference,
                then_block: analyzed_then_block,
                else_block: analyzed_else_block,
                type_: if_expression.get_type(),
                position: if_expression.get_position(),
            }),
        );

        Ok(AnalyzedExpressionRef::for_expression(i, if_expression))
    }

    fn analyze_infix_operation(
        &mut self,
        infix_operation: &TypedInfixOperation,
    ) -> Result<AnalyzedExpressionRef, CompilationError> {
        let left_reference = self.analyze_expression(&infix_operation.left)?;
        let right_reference = self.analyze_expression(&infix_operation.right)?;
        let i = self
            .expression_graph
            .add_node(AnalyzedExpression::InfixOperation(AnalyzedInfixOperation {
                left: left_reference.clone(),
                operator: infix_operation.operator,
                right: right_reference.clone(),
                type_: infix_operation.type_.clone(),
                position: infix_operation.get_position(),
            }));

        self.expression_graph
            .add_edge_with_reference(i, &left_reference);

        self.expression_graph
            .add_edge_with_reference(i, &right_reference);

        Ok(AnalyzedExpressionRef::for_expression(i, infix_operation))
    }

    fn analyze_prefix_operation(
        &mut self,
        prefix_operation: &TypedPrefixOperation,
    ) -> Result<AnalyzedExpressionRef, CompilationError> {
        let expression_reference = self.analyze_expression(&prefix_operation.expression)?;
        let i = self
            .expression_graph
            .add_node(AnalyzedExpression::PrefixOperation(
                AnalyzedPrefixOperation {
                    operator: prefix_operation.operator,
                    expression: expression_reference.clone(),
                    type_: prefix_operation.get_type(),
                    position: prefix_operation.get_position(),
                },
            ));

        self.expression_graph
            .add_edge_with_reference(i, &expression_reference);

        Ok(AnalyzedExpressionRef::for_expression(i, prefix_operation))
    }

    fn analyze_statement(&mut self, statement: &TypedStatement) -> Result<(), CompilationError> {
        match statement {
            TypedStatement::VariableDefinition(definition) => {
                let analyzed = self.analyze_expression(&definition.value)?;

                self.scope.declare_variable(&definition.name, analyzed)?;

                Ok(())
            }

            TypedStatement::FunctionDefinition(definition) => Err(CompilationError {
                message: CompilationErrorMessage::NestedFunctionsNotSupported,
                position: Some(definition.get_position()),
            }),

            TypedStatement::Expression(expression) => {
                self.analyze_expression(expression)?;

                Ok(())
            }

            TypedStatement::NoOp { .. } => Ok(()),
        }
    }

    fn analyze_statements_with_functions_hoisted(
        &mut self,
        statements: &[TypedStatement],
    ) -> Result<(), CompilationError> {
        let mut function_indices = Vec::new();

        for statement in statements {
            if let TypedStatement::FunctionDefinition(definition) = statement {
                if self.scope.has_parent() {
                    return Err(CompilationError {
                        message: CompilationErrorMessage::NestedFunctionsNotSupported,
                        position: Some(definition.get_position()),
                    });
                }

                let i = self.functions.reserve_function();

                function_indices.push(i);

                self.scope.declare_variable(
                    &definition.name,
                    AnalyzedExpressionRef::Function {
                        index: i,
                        type_: definition.get_type(),
                        position: definition.get_position(),
                    },
                )?;
            }
        }

        /*
         * Analyze the functions first, so they don't yet have access to global variables. The
         * typechecker should've already prevented this, but it never hurts to check again.
         */
        for (i, statement) in statements.iter().enumerate() {
            if let TypedStatement::FunctionDefinition(definition) = statement {
                self.analyze_function_definition(function_indices[i], definition)?;
            }
        }

        for statement in statements {
            if let TypedStatement::FunctionDefinition(_) = statement {
            } else {
                self.analyze_statement(statement)?;
            }
        }

        Ok(())
    }

    fn into_expression_graph(self) -> DirectedGraph<AnalyzedExpression> {
        self.expression_graph.into_graph()
    }

    fn with_parent(parent: &'a mut Analyzer, surrounding_expression: Option<usize>) -> Self {
        Self {
            scope: AnalyzerScope::with_parent(&parent.scope),
            functions: parent.functions,
            expression_graph: AnalyzerExpressionGraph::with_parent(
                &mut parent.expression_graph,
                surrounding_expression,
            ),
        }
    }

    fn without_parent(
        built_in_values: &'a BuiltInValues,
        functions: &'a mut AnalyzerFunctions,
    ) -> Self {
        Self {
            scope: AnalyzerScope::without_parent(built_in_values),
            functions,
            expression_graph: AnalyzerExpressionGraph::without_parent(),
        }
    }
}

struct AnalyzerExpressionGraph<'a> {
    parent_add_edge: Option<Box<dyn FnMut(usize, usize) + 'a>>,
    surrounding_expression: Option<usize>,
    expressions: DirectedGraph<AnalyzedExpression>,
}

impl<'a> AnalyzerExpressionGraph<'a> {
    fn add_edge(&mut self, dependent: usize, dependency: usize) {
        match self.parent_add_edge.as_mut() {
            Some(parent_add_edge) if dependency < self.expressions.offset => {
                parent_add_edge(self.surrounding_expression.unwrap(), dependency);
            }

            _ => self.expressions.add_edge(dependent, dependency),
        }
    }

    fn add_edge_with_reference(&mut self, dependent: usize, dependency: &AnalyzedExpressionRef) {
        match dependency {
            AnalyzedExpressionRef::Expression { index, .. } => {
                self.add_edge(dependent, *index);
            }

            _ => {}
        }
    }

    fn add_node(&mut self, node: AnalyzedExpression) -> usize {
        self.expressions.add_node(node)
    }

    fn into_graph(self) -> DirectedGraph<AnalyzedExpression> {
        self.expressions
    }

    fn reserve_node(&mut self) -> usize {
        self.expressions.reserve_node()
    }

    fn set_node(&mut self, i: usize, node: AnalyzedExpression) {
        self.expressions.set_node(i, node);
    }

    fn with_parent(
        parent: &'a mut AnalyzerExpressionGraph,
        surrounding_expression: Option<usize>,
    ) -> Self {
        let offset = match parent.expressions.last() {
            Some((i, _)) => i + 1,
            None => parent.expressions.offset,
        };

        Self {
            parent_add_edge: Some(Box::new(|i, j| parent.add_edge(i, j))),
            surrounding_expression,
            expressions: DirectedGraph::new(offset),
        }
    }

    fn without_parent() -> Self {
        Self {
            parent_add_edge: None,
            surrounding_expression: None,
            expressions: DirectedGraph::new(0),
        }
    }
}

struct AnalyzerFunctions {
    function_count: usize,
    functions: NumberMap<AnalyzedFunctionDefinition>,
}

impl AnalyzerFunctions {
    fn new() -> Self {
        Self {
            function_count: 0,
            functions: NumberMap::new(0),
        }
    }

    fn into_vec(self) -> Result<Vec<AnalyzedFunctionDefinition>, CompilationError> {
        let mut result = Vec::with_capacity(self.function_count);
        let mut i = 0;

        for (j, definition) in self.functions.into_iter() {
            if j > i {
                return Err(CompilationError {
                    message: CompilationErrorMessage::InternalError(
                        InternalError::AnalyzerFunctionNotDefined { index: i },
                    ),
                    position: None,
                });
            }

            result.push(definition);

            i += 1;
        }

        if result.len() < self.function_count {
            return Err(CompilationError {
                message: CompilationErrorMessage::InternalError(
                    InternalError::AnalyzerFunctionNotDefined {
                        index: result.len(),
                    },
                ),
                position: None,
            });
        }

        Ok(result)
    }

    fn reserve_function(&mut self) -> usize {
        let result = self.function_count;

        self.function_count += 1;

        result
    }

    fn set_function(&mut self, i: usize, definition: AnalyzedFunctionDefinition) {
        self.functions.insert(i, definition);
    }
}

pub struct AnalyzerPhase;

impl Phase<&TypedProgram, AnalyzedProgram, CompilationError> for AnalyzerPhase {
    fn name() -> String {
        String::from("analyzer")
    }

    fn execute(&self, program: &TypedProgram) -> Result<AnalyzedProgram, CompilationError> {
        let mut functions = AnalyzerFunctions::new();
        let mut analyzer = Analyzer::without_parent(BuiltInValues::global(), &mut functions);

        analyzer.analyze_statements_with_functions_hoisted(&program.statements)?;

        let expression_graph = analyzer.into_expression_graph();

        Ok(AnalyzedProgram {
            functions: functions.into_vec()?,
            expression_graph: expression_graph,
            position: program.get_position(),
        })
    }
}
