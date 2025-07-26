pub mod analyzed_program;
mod scope;

use crate::{
    data_structures::{graph::DirectedGraph, number_map::NumberMap},
    dependency_analyzer::{
        analyzed_program::{
            AnalyzedBlock, AnalyzedCall, AnalyzedExpression, AnalyzedExpressionRef,
            AnalyzedFunctionDefinition, AnalyzedIf, AnalyzedInfixOperation, AnalyzedNumber,
            AnalyzedParameter, AnalyzedPrefixOperation, AnalyzedProgram, AnalyzedSelect,
            AnalyzedStructApplication, AnalyzedStructApplicationField, AnalyzedType, AnalyzedUnit,
        },
        scope::{AnalyzerScope, AnalyzerScopeContext},
    },
    error_reporting::{CompilationError, CompilationErrorMessage, InternalError, Position},
    parser::{program::Node, type_::Type},
    phase::Phase,
    typechecker::typed_program::{
        TypedBlock, TypedCall, TypedExpression, TypedFunctionDefinition, TypedIdentifier, TypedIf,
        TypedInfixOperation, TypedNumber, TypedPrefixOperation, TypedProgram, TypedSelect,
        TypedStatement, TypedStructApplication, TypedUnit,
    },
};

struct Analyzer<'a> {
    scope_context: &'a mut AnalyzerScopeContext,
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

        analyzer.analyze_statements_with_hoisting(&block.statements, false)?;

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

        let (i, result) = self
            .expression_graph
            .add_node(AnalyzedExpression::Call(AnalyzedCall {
                function: function_reference.clone(),
                arguments: argument_references.clone(),
                type_: self.scope.analyze_type(&call.type_)?,
                position: call.get_position(),
            }));

        self.expression_graph
            .add_edge_with_reference(i, &function_reference);

        for reference in &argument_references {
            self.expression_graph.add_edge_with_reference(i, reference);
        }

        Ok(result)
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

            TypedExpression::Select(select) => self.analyze_select(select),
            TypedExpression::Call(call) => self.analyze_call(call),
            TypedExpression::StructApplication(struct_application) => {
                self.analyze_struct_application(struct_application)
            }

            TypedExpression::Identifier(identifier) => self.analyze_identifier(identifier),
            TypedExpression::Number(number) => {
                Ok(AnalyzedExpressionRef::Number(self.analyze_number(number)))
            }

            TypedExpression::PrefixOperation(prefix_operation) => {
                self.analyze_prefix_operation(prefix_operation)
            }

            TypedExpression::SequentialBlock(block) => {
                let last_statement_index =
                    self.analyze_statements_with_hoisting(&block.statements, true)?;

                let result_reference = match &block.result {
                    Some(result) => Some(self.analyze_expression(result)?),
                    None => None,
                };

                if let (
                    Some(AnalyzedExpressionRef::Expression {
                        index: result_index,
                        ..
                    }),
                    Some(last_statement_index),
                ) = (&result_reference, last_statement_index)
                {
                    self.expression_graph
                        .add_edge(last_statement_index, *result_index);
                }

                Ok(result_reference.unwrap_or_else(|| {
                    let i = block.get_position().0.end;

                    AnalyzedExpressionRef::Unit(AnalyzedUnit {
                        position: Position(i..i),
                    })
                }))
            }

            TypedExpression::Unit(unit) => Ok(AnalyzedExpressionRef::Unit(self.analyze_unit(unit))),
        }
    }

    fn analyze_function_definition(
        &mut self,
        i: usize,
        definition: &TypedFunctionDefinition,
    ) -> Result<(), CompilationError> {
        let mut analyzed_parameters = Vec::with_capacity(definition.parameters.len());

        for parameter in &definition.parameters {
            let analyzed_type = self.scope.analyze_type(&parameter.type_)?;

            analyzed_parameters.push(AnalyzedParameter {
                name: parameter.underlying.value.clone(),
                type_: analyzed_type.clone(),
                position: parameter.get_position(),
            });
        }

        let mut analyzer = Analyzer::with_parent(self, None);

        for (j, parameter) in definition.parameters.iter().enumerate() {
            analyzer.scope.declare_variable(
                &parameter.underlying,
                AnalyzedExpressionRef::Parameter {
                    index: j,
                    type_: analyzed_parameters[j].type_.clone(),
                    position: parameter.get_position(),
                },
            )?;
        }

        analyzer.analyze_statements_with_hoisting(&definition.body.statements, false)?;

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
                parameters: analyzed_parameters,
                body: analyzed_body,
                type_: self.scope.analyze_function_type(&definition.type_)?,
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
            .get_variable(identifier)?
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

        Ok(self.expression_graph.set_node(
            i,
            AnalyzedExpression::If(AnalyzedIf {
                condition: condition_reference,
                then_block: analyzed_then_block,
                else_block: analyzed_else_block,
                type_: self.scope.analyze_type(&if_expression.type_)?,
                position: if_expression.get_position(),
            }),
        ))
    }

    fn analyze_infix_operation(
        &mut self,
        infix_operation: &TypedInfixOperation,
    ) -> Result<AnalyzedExpressionRef, CompilationError> {
        let left_reference = self.analyze_expression(&infix_operation.left)?;
        let right_reference = self.analyze_expression(&infix_operation.right)?;
        let (i, result) = self
            .expression_graph
            .add_node(AnalyzedExpression::InfixOperation(AnalyzedInfixOperation {
                left: left_reference.clone(),
                operator: infix_operation.operator,
                right: right_reference.clone(),
                type_: self.scope.analyze_type(&infix_operation.type_)?,
                position: infix_operation.get_position(),
            }));

        self.expression_graph
            .add_edge_with_reference(i, &left_reference);

        self.expression_graph
            .add_edge_with_reference(i, &right_reference);

        Ok(result)
    }

    fn analyze_number(&self, number: &TypedNumber) -> AnalyzedNumber {
        AnalyzedNumber {
            value: number.value,
            type_: number.type_,
            position: number.get_position(),
        }
    }

    fn analyze_prefix_operation(
        &mut self,
        prefix_operation: &TypedPrefixOperation,
    ) -> Result<AnalyzedExpressionRef, CompilationError> {
        let expression_reference = self.analyze_expression(&prefix_operation.expression)?;
        let (i, result) = self
            .expression_graph
            .add_node(AnalyzedExpression::PrefixOperation(
                AnalyzedPrefixOperation {
                    operator: prefix_operation.operator,
                    expression: expression_reference.clone(),
                    type_: self.scope.analyze_type(&prefix_operation.type_)?,
                    position: prefix_operation.get_position(),
                },
            ));

        self.expression_graph
            .add_edge_with_reference(i, &expression_reference);

        Ok(result)
    }

    fn analyze_select(
        &mut self,
        select: &TypedSelect,
    ) -> Result<AnalyzedExpressionRef, CompilationError> {
        let analyzed_left = self.analyze_expression(&select.left_hand_side)?;
        let (i, result) =
            self.expression_graph
                .add_node(AnalyzedExpression::Select(AnalyzedSelect {
                    left_hand_side: analyzed_left.clone(),
                    field_index: select.field_index,
                    type_: self.scope.analyze_type(&select.type_)?,
                    position: select.get_position(),
                }));

        self.expression_graph
            .add_edge_with_reference(i, &analyzed_left);

        Ok(result)
    }

    fn analyze_statement(
        &mut self,
        statement: &TypedStatement,
    ) -> Result<Option<AnalyzedExpressionRef>, CompilationError> {
        match statement {
            // `analyze_statements_with_hoisting` should've already analyzed the struct definitions
            TypedStatement::StructDefinition(_) => Ok(None),
            TypedStatement::VariableDefinition(definition) => {
                let analyzed = self.analyze_expression(&definition.value)?;

                self.scope
                    .declare_variable(&definition.name, analyzed.clone())?;

                Ok(Some(analyzed))
            }

            TypedStatement::FunctionDefinition(definition) => Err(CompilationError {
                message: CompilationErrorMessage::NestedFunctionsNotSupported,
                position: Some(definition.get_position()),
            }),

            TypedStatement::Expression(expression) => {
                Ok(Some(self.analyze_expression(expression)?))
            }

            TypedStatement::NoOp { .. } => Ok(None),
        }
    }

    fn analyze_statements_with_hoisting(
        &mut self,
        statements: &[TypedStatement],
        sequential: bool,
    ) -> Result<Option<usize>, CompilationError> {
        let mut struct_indices = Vec::new();
        let mut function_indices = Vec::new();

        for statement in statements {
            match statement {
                TypedStatement::StructDefinition(definition) => {
                    struct_indices.push(
                        self.scope
                            .declare_struct(definition.name.value.clone(), self.scope_context),
                    );
                }

                TypedStatement::FunctionDefinition(definition) => {
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
                            type_: self
                                .scope
                                .analyze_type(&Type::Function(definition.type_.clone()))?,

                            position: definition.get_position(),
                        },
                    )?;
                }

                _ => {}
            }
        }

        for (i, definition) in statements
            .iter()
            .filter_map(|statement| match statement {
                TypedStatement::StructDefinition(definition) => Some(definition),
                _ => None,
            })
            .enumerate()
        {
            self.scope
                .define_struct(struct_indices[i], definition, self.scope_context)?;
        }

        /*
         * Analyze the functions first, so they don't yet have access to global variables. The
         * typechecker should've already prevented this, but it never hurts to check again.
         */
        for (i, definition) in statements
            .iter()
            .filter_map(|statement| match statement {
                TypedStatement::FunctionDefinition(definition) => Some(definition),
                _ => None,
            })
            .enumerate()
        {
            self.analyze_function_definition(function_indices[i], definition)?
        }

        let mut last_index = None;

        for statement in statements {
            if let TypedStatement::FunctionDefinition(_) = statement {
            } else {
                let next_index = match self.analyze_statement(statement)? {
                    Some(AnalyzedExpressionRef::Expression { index, .. }) => Some(index),
                    _ => None,
                };

                match (last_index, next_index) {
                    (Some(last_index), Some(next_index)) if sequential => {
                        self.expression_graph.add_edge(next_index, last_index);
                    }

                    _ => {}
                }

                if next_index.is_some() {
                    last_index = next_index;
                }
            }
        }

        Ok(last_index)
    }

    fn analyze_struct_application(
        &mut self,
        struct_application: &TypedStructApplication,
    ) -> Result<AnalyzedExpressionRef, CompilationError> {
        let mut analyzed_fields = Vec::with_capacity(struct_application.fields.len());

        for field in &struct_application.fields {
            analyzed_fields.push(AnalyzedStructApplicationField {
                name: field.name.value.clone(),
                value: self.analyze_expression(&field.value)?,
            });
        }

        let i = self.expression_graph.reserve_node();

        for analyzed_field in &analyzed_fields {
            self.expression_graph
                .add_edge_with_reference(i, &analyzed_field.value);
        }

        let struct_index = self
            .scope
            .get_struct_index(&struct_application.name.value)
            .ok_or_else(|| CompilationError {
                message: CompilationErrorMessage::UnknownType {
                    name: struct_application.name.value.clone(),
                },
                position: Some(struct_application.name.get_position()),
            })?;

        Ok(self.expression_graph.set_node(
            i,
            AnalyzedExpression::StructApplication(AnalyzedStructApplication {
                struct_index,
                fields: analyzed_fields,
                // We probably should call `self.scope.analyze_type` here, but this is faster
                type_: AnalyzedType::Struct(struct_index),
                position: struct_application.get_position(),
            }),
        ))
    }

    fn analyze_unit(&self, unit: &TypedUnit) -> AnalyzedUnit {
        AnalyzedUnit {
            position: unit.get_position(),
        }
    }

    fn into_expression_graph(self) -> DirectedGraph<AnalyzedExpression> {
        self.expression_graph.into_graph()
    }

    fn with_parent(parent: &'a mut Analyzer, surrounding_expression: Option<usize>) -> Self {
        Self {
            scope_context: parent.scope_context,
            scope: AnalyzerScope::with_parent(&parent.scope),
            functions: parent.functions,
            expression_graph: AnalyzerExpressionGraph::with_parent(
                &mut parent.expression_graph,
                surrounding_expression,
            ),
        }
    }

    fn without_parent(
        scope_context: &'a mut AnalyzerScopeContext,
        functions: &'a mut AnalyzerFunctions,
    ) -> Self {
        Self {
            scope_context,
            scope: AnalyzerScope::without_parent(),
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
            Some(parent_add_edge) if dependency < self.expressions.get_offset() => {
                parent_add_edge(self.surrounding_expression.unwrap(), dependency);
            }

            _ => self.expressions.add_edge(dependent, dependency),
        }
    }

    fn add_edge_with_reference(&mut self, dependent: usize, dependency: &AnalyzedExpressionRef) {
        if let AnalyzedExpressionRef::Expression { index, .. } = dependency {
            self.add_edge(dependent, *index);
        }
    }

    fn add_node(&mut self, node: AnalyzedExpression) -> (usize, AnalyzedExpressionRef) {
        let (i, node_reference) = self.expressions.add_node(node);

        (i, AnalyzedExpressionRef::for_expression(i, node_reference))
    }

    fn get_next_node(&self) -> usize {
        self.expressions.get_next_node()
    }

    fn into_graph(self) -> DirectedGraph<AnalyzedExpression> {
        self.expressions
    }

    fn reserve_node(&mut self) -> usize {
        self.expressions.reserve_node()
    }

    fn set_node(&mut self, i: usize, node: AnalyzedExpression) -> AnalyzedExpressionRef {
        let node_reference = self.expressions.set_node(i, node);

        AnalyzedExpressionRef::for_expression(i, node_reference)
    }

    fn with_parent(
        parent: &'a mut AnalyzerExpressionGraph,
        surrounding_expression: Option<usize>,
    ) -> Self {
        let offset = parent.get_next_node();

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
    fn into_vec(self) -> Result<Vec<AnalyzedFunctionDefinition>, CompilationError> {
        self.functions
            .into_contiguous_values(self.function_count)
            .map_err(|error| CompilationError {
                message: CompilationErrorMessage::InternalError(
                    InternalError::AnalyzerFunctionNotDefined { index: error.index },
                ),
                position: None,
            })
    }

    fn new() -> Self {
        Self {
            function_count: 0,
            functions: NumberMap::new(0),
        }
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
        let mut analyzer_scope_context = AnalyzerScopeContext::new();
        let mut analyzer = Analyzer::without_parent(&mut analyzer_scope_context, &mut functions);

        analyzer.analyze_statements_with_hoisting(&program.statements, false)?;

        let expression_graph = analyzer.into_expression_graph();

        Ok(AnalyzedProgram {
            structs: analyzer_scope_context.into_structs()?,
            functions: functions.into_vec()?,
            expression_graph,
            position: program.get_position(),
        })
    }
}
