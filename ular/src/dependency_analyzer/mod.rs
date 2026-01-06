pub(crate) mod analyzed_program;
mod scope;

use crate::{
    arguments::PhaseName,
    data_structures::{
        graph::DirectedGraph,
        number_map::{IndexUndefinedError, NumberMap},
    },
    dependency_analyzer::{
        analyzed_program::{
            AnalyzedBlock, AnalyzedCall, AnalyzedClosure, AnalyzedExpression,
            AnalyzedExpressionGraphRef, AnalyzedExpressionRef, AnalyzedFunctionDefinition,
            AnalyzedIf, AnalyzedInfixOperation, AnalyzedNumber, AnalyzedParameter,
            AnalyzedPrefixOperation, AnalyzedProgram, AnalyzedSelect, AnalyzedStringLiteral,
            AnalyzedStructApplication, AnalyzedStructApplicationField, AnalyzedStructDefinition,
            AnalyzedStructDefinitionField, AnalyzedType, AnalyzedUnit,
        },
        scope::{AnalyzerScope, AnalyzerScopeContext},
    },
    error_reporting::{CompilationError, CompilationErrorMessage, InternalError, Position},
    parser::{
        program::{Node, StringLiteral},
        type_::Type,
    },
    phase::Phase,
    typechecker::{
        built_in_values::{TypecheckerBuiltInValueProducer, TypecheckerBuiltInValues},
        typed_program::{
            TypedBlock, TypedCall, TypedClosure, TypedExpression, TypedFunctionDefinition, TypedIf,
            TypedInfixOperation, TypedNumber, TypedPath, TypedPrefixOperation, TypedProgram,
            TypedSelect, TypedStatement, TypedStructApplication, TypedStructDefinition, TypedUnit,
        },
    },
};
use std::{collections::HashMap, ops::Range};

struct Analyzer<'parent, 'phase> {
    scope_context: &'phase mut AnalyzerScopeContext,
    scope: AnalyzerScope<'phase>,
    expression_graph: AnalyzerExpressionGraph<'parent>,
}

impl<'phase> Analyzer<'_, 'phase> {
    fn analyze_block(
        &mut self,
        block: &TypedBlock,
        surrounding_expression: usize,
    ) -> Result<AnalyzedBlock, CompilationError> {
        let mut analyzer = self.new_child(Some(surrounding_expression));

        analyzer.analyze_statements_with_hoisting(&block.statements, false)?;

        let result_reference = match &block.result {
            Some(result) => Some(analyzer.analyze_expression(result)?),
            None => None,
        };

        Ok(AnalyzedBlock {
            expression_graph: analyzer.into_expression_graph(),
            result: result_reference,
            position: block.get_position(),
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

    fn analyze_closure(
        &mut self,
        closure: &TypedClosure,
    ) -> Result<AnalyzedExpressionRef, CompilationError> {
        let expression_index = self.expression_graph.reserve_node();
        let mut analyzed_parameters = Vec::with_capacity(closure.parameters.len());

        for parameter in &closure.parameters {
            let analyzed_type = self.scope.analyze_type(&parameter.type_)?;

            analyzed_parameters.push(AnalyzedParameter {
                name: parameter.underlying.value.clone(),
                type_: analyzed_type.clone(),
                position: parameter.get_position(),
            });
        }

        let mut parent_expression_graph =
            AnalyzerClosureExpressionGraphParent::new(&mut self.expression_graph, expression_index);

        let mut analyzer = Analyzer {
            scope_context: self.scope_context,
            scope: AnalyzerScope::with_parent(&self.scope),
            expression_graph: parent_expression_graph.new_child(Some(expression_index)),
        };

        for (i, parameter) in closure.parameters.iter().enumerate() {
            analyzer.scope.declare_variable(
                &parameter.underlying,
                AnalyzedExpressionRef::Parameter {
                    index: i,
                    type_: analyzed_parameters[i].type_.clone(),
                    position: parameter.get_position(),
                },
            )?;
        }

        analyzer.analyze_statements_with_hoisting(&closure.body.statements, false)?;

        let result_reference = match &closure.body.result {
            Some(result) => Some(analyzer.analyze_expression(result)?),
            None => None,
        };

        let expression_graph = analyzer.into_expression_graph();
        let analyzed_body = AnalyzedBlock {
            expression_graph,
            result: result_reference,
            position: closure.body.get_position(),
        };

        let dependencies = parent_expression_graph.dependencies;
        let function_type = self.scope.analyze_function_type(&closure.type_)?;

        Ok(self.expression_graph.set_node(
            expression_index,
            AnalyzedExpression::Closure(AnalyzedClosure {
                parameters: analyzed_parameters,
                body: analyzed_body,
                dependencies,
                type_: function_type,
                position: closure.get_position(),
            }),
        ))
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
            TypedExpression::Closure(closure) => self.analyze_closure(closure),
            TypedExpression::StructApplication(struct_application) => {
                self.analyze_struct_application(struct_application)
            }

            TypedExpression::Path(path) => self.analyze_path(path),
            TypedExpression::Number(number) => {
                Ok(AnalyzedExpressionRef::Number(self.analyze_number(number)))
            }

            TypedExpression::String(string) => Ok(self.analyze_string(string)),
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
                    Some(AnalyzedExpressionRef::Expression(AnalyzedExpressionGraphRef {
                        index: result_index,
                        ..
                    })),
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
        definition: &TypedFunctionDefinition,
    ) -> Result<AnalyzedFunctionDefinition, CompilationError> {
        let mut analyzed_parameters = Vec::with_capacity(definition.parameters.len());

        for parameter in &definition.parameters {
            let analyzed_type = self.scope.analyze_type(&parameter.type_)?;

            analyzed_parameters.push(AnalyzedParameter {
                name: parameter.underlying.value.clone(),
                type_: analyzed_type.clone(),
                position: parameter.get_position(),
            });
        }

        let mut analyzer = self.new_child(None);

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
            position: definition.body.get_position(),
        };

        Ok(AnalyzedFunctionDefinition {
            name: definition.name.clone(),
            parameters: analyzed_parameters,
            body: analyzed_body,
            type_: self.scope.analyze_function_type(&definition.type_)?,
            position: definition.get_position(),
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

    fn analyze_path(&self, path: &TypedPath) -> Result<AnalyzedExpressionRef, CompilationError> {
        match path {
            TypedPath::BuiltIn {
                underlying,
                type_,
                position,
            } => Ok(AnalyzedExpressionRef::BuiltIn {
                path: underlying.clone(),
                type_: self.scope.analyze_type(type_)?,
                position: position.clone(),
            }),

            TypedPath::UserDefinedIdentifier(identifier) => self
                .scope
                .get_variable(identifier)?
                .ok_or_else(|| CompilationError {
                    message: CompilationErrorMessage::InternalError(InternalError::UnknownValue {
                        name: identifier.underlying.value.clone(),
                    }),

                    position: Some(identifier.get_position()),
                }),

            TypedPath::UserDefinedMethod {
                left_hand_side,
                method_index,
                type_,
                position,
            } => {
                let struct_index = self
                    .scope
                    .get_struct_index(&left_hand_side.value)
                    .ok_or_else(|| CompilationError {
                        message: CompilationErrorMessage::InternalError(
                            InternalError::UnknownType {
                                name: left_hand_side.value.clone(),
                            },
                        ),

                        position: Some(left_hand_side.get_position()),
                    })?;

                Ok(AnalyzedExpressionRef::StructMethod {
                    struct_index,
                    method_index: *method_index,
                    type_: self.scope.analyze_type(type_)?,
                    position: position.clone(),
                })
            }
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
            /*
             * `analyze_statements_with_hoisting` should've already analyzed the struct and function
             * definitions
             */
            TypedStatement::StructDefinition(_) | TypedStatement::FunctionDefinition(_) => Ok(None),
            TypedStatement::VariableDefinition(definition) => {
                let analyzed = self.analyze_expression(&definition.value)?;

                self.scope
                    .declare_variable(&definition.name, analyzed.clone())?;

                Ok(Some(analyzed))
            }

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

        // "Hoist" the structs and functions by registering them with the scope first, so they're
        // accessible in the rest of the block regardless of where or in what order they're declared
        for statement in statements {
            match statement {
                TypedStatement::StructDefinition(definition) => {
                    struct_indices.push(
                        self.scope
                            .declare_struct(definition.name.value.clone(), self.scope_context),
                    );
                }

                TypedStatement::FunctionDefinition(definition) => {
                    let i = self.scope_context.functions_mut().reserve_definition();

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
            let analyzed_definition = self.analyze_struct_definition(definition)?;

            self.scope
                .define_struct(struct_indices[i], analyzed_definition, self.scope_context);
        }

        for (i, definition) in statements
            .iter()
            .filter_map(|statement| match statement {
                TypedStatement::FunctionDefinition(definition) => Some(definition),
                _ => None,
            })
            .enumerate()
        {
            let analyzed_definition = self.analyze_function_definition(definition)?;

            self.scope_context
                .functions_mut()
                .set_definition(function_indices[i], analyzed_definition);
        }

        let mut last_index = None;

        for statement in statements {
            if let TypedStatement::StructDefinition(_) | TypedStatement::FunctionDefinition(_) =
                statement
            {
            } else {
                let next_index = match self.analyze_statement(statement)? {
                    Some(AnalyzedExpressionRef::Expression(expression_graph_ref)) => {
                        Some(expression_graph_ref.index)
                    }

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

    fn analyze_string(&mut self, string: &StringLiteral) -> AnalyzedExpressionRef {
        let (_, result) =
            self.expression_graph
                .add_node(AnalyzedExpression::String(AnalyzedStringLiteral {
                    index: self.scope_context.add_string_literal(string.value.clone()),
                    position: string.position.clone(),
                }));

        result
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
                position: field.get_position(),
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
                message: CompilationErrorMessage::InternalError(InternalError::UnknownType {
                    name: struct_application.name.value.clone(),
                }),

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

    fn analyze_struct_definition(
        &mut self,
        definition: &TypedStructDefinition,
    ) -> Result<AnalyzedStructDefinition, CompilationError> {
        Ok(AnalyzedStructDefinition {
            name: definition.name.clone(),
            fields: definition
                .fields
                .iter()
                .map(|field| {
                    Ok(AnalyzedStructDefinitionField {
                        name: field.name.clone(),
                        type_: self.scope.analyze_type(&field.type_)?,
                        position: field.get_position(),
                    })
                })
                .collect::<Result<_, _>>()?,

            methods: definition
                .methods
                .iter()
                .map(|method| self.analyze_function_definition(method))
                .collect::<Result<_, _>>()?,

            position: definition.position.clone(),
        })
    }

    fn analyze_unit(&self, unit: &TypedUnit) -> AnalyzedUnit {
        AnalyzedUnit {
            position: unit.get_position(),
        }
    }

    fn into_expression_graph(self) -> DirectedGraph<AnalyzedExpression> {
        self.expression_graph.into_graph()
    }

    fn new(
        built_in_values: &'phase TypecheckerBuiltInValues,
        scope_context: &'phase mut AnalyzerScopeContext,
    ) -> Self {
        Self {
            scope_context,
            scope: AnalyzerScope::without_parent(built_in_values),
            expression_graph: AnalyzerExpressionGraph::new(),
        }
    }

    fn new_child(&mut self, surrounding_expression: Option<usize>) -> Analyzer<'_, '_> {
        Analyzer {
            scope_context: self.scope_context,
            scope: AnalyzerScope::with_parent(&self.scope),
            expression_graph: self.expression_graph.new_child(surrounding_expression),
        }
    }
}

struct AnalyzerExpressionGraph<'a> {
    parent: Option<&'a mut dyn AnalyzerExpressionGraphParent>,
    surrounding_expression: Option<usize>,
    expressions: DirectedGraph<AnalyzedExpression>,
}

impl AnalyzerExpressionGraph<'_> {
    fn new() -> Self {
        Self {
            parent: None,
            surrounding_expression: None,
            expressions: DirectedGraph::new(0),
        }
    }
}

impl AnalyzerExpressionGraph<'_> {
    fn add_edge_with_reference(&mut self, dependent: usize, dependency: &AnalyzedExpressionRef) {
        if let AnalyzedExpressionRef::Expression(expression_graph_ref) = dependency {
            self.add_edge(dependent, expression_graph_ref.index);
        }
    }

    fn add_edge(&mut self, dependent: usize, dependency: usize) {
        match self.parent.as_mut() {
            Some(parent) if dependency < self.expressions.get_offset() => {
                parent.add_edge(self.surrounding_expression.unwrap(), dependency);
            }

            _ => self.expressions.add_edge(dependent, dependency),
        }
    }

    fn add_node(&mut self, node: AnalyzedExpression) -> (usize, AnalyzedExpressionRef) {
        let (i, node_reference) = self.expressions.add_node(node);

        (i, AnalyzedExpressionRef::for_expression(i, node_reference))
    }

    fn get_next_node(&self) -> usize {
        self.expressions.get_next_node()
    }

    fn get_node_reference(&self, i: usize) -> Option<AnalyzedExpressionGraphRef> {
        self.expressions
            .get_node(i)
            .map(|node_reference| AnalyzedExpressionGraphRef::for_expression(i, node_reference))
            .or_else(|| {
                self.parent
                    .as_ref()
                    .and_then(|parent| parent.get_node_reference(i))
            })
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
}

impl AnalyzerExpressionGraphParent for AnalyzerExpressionGraph<'_> {
    fn add_edge(&mut self, dependent: usize, dependency: usize) {
        self.add_edge(dependent, dependency);
    }

    fn as_mut_dyn_ref(&mut self) -> &mut dyn AnalyzerExpressionGraphParent {
        self
    }

    fn get_next_node(&self) -> usize {
        self.get_next_node()
    }

    fn get_node_reference(&self, i: usize) -> Option<AnalyzedExpressionGraphRef> {
        self.get_node_reference(i)
    }
}

struct AnalyzerClosureExpressionGraphParent<'a> {
    parent: &'a mut dyn AnalyzerExpressionGraphParent,
    dependencies: Vec<AnalyzedExpressionGraphRef>,
    closure_expression_index: usize,
}

impl<'a> AnalyzerClosureExpressionGraphParent<'a> {
    fn new(
        parent: &'a mut dyn AnalyzerExpressionGraphParent,
        closure_expression_index: usize,
    ) -> Self {
        Self {
            parent,
            dependencies: Vec::new(),
            closure_expression_index,
        }
    }
}

impl AnalyzerExpressionGraphParent for AnalyzerClosureExpressionGraphParent<'_> {
    fn add_edge(&mut self, dependent: usize, dependency: usize) {
        assert_eq!(dependent, self.closure_expression_index);

        self.dependencies
            .push(self.parent.get_node_reference(dependency).unwrap());

        self.parent
            .add_edge(self.closure_expression_index, dependency);
    }

    fn as_mut_dyn_ref(&mut self) -> &mut dyn AnalyzerExpressionGraphParent {
        self
    }

    fn get_next_node(&self) -> usize {
        self.parent.get_next_node()
    }

    fn get_node_reference(&self, i: usize) -> Option<AnalyzedExpressionGraphRef> {
        self.parent.get_node_reference(i)
    }
}

trait AnalyzerExpressionGraphParent {
    fn add_edge(&mut self, dependent: usize, dependency: usize);
    fn as_mut_dyn_ref(&mut self) -> &mut dyn AnalyzerExpressionGraphParent;
    fn get_next_node(&self) -> usize;
    fn get_node_reference(&self, i: usize) -> Option<AnalyzedExpressionGraphRef>;

    fn new_child(&mut self, surrounding_expression: Option<usize>) -> AnalyzerExpressionGraph<'_> {
        let offset = self.get_next_node();

        AnalyzerExpressionGraph {
            parent: Some(self.as_mut_dyn_ref()),
            surrounding_expression,
            expressions: DirectedGraph::new(offset),
        }
    }
}

pub(crate) struct AnalyzerPhase {
    pub(crate) additional_values: HashMap<String, Type>,
}

impl Phase<&TypedProgram> for AnalyzerPhase {
    type Output = AnalyzedProgram;

    fn name() -> PhaseName {
        PhaseName::Analyzer
    }

    fn execute(&self, program: &TypedProgram) -> Result<AnalyzedProgram, CompilationError> {
        let built_in_values = TypecheckerBuiltInValues::new(
            TypecheckerBuiltInValueProducer,
            self.additional_values.clone(),
        );

        let mut analyzer_scope_context = AnalyzerScopeContext::new();
        let mut analyzer = Analyzer::new(&built_in_values, &mut analyzer_scope_context);

        analyzer.analyze_statements_with_hoisting(&program.statements, false)?;

        let expression_graph = analyzer.into_expression_graph();

        analyzer_scope_context.into_program(expression_graph, program.get_position())
    }
}

pub(crate) struct DefinitionMap<A> {
    definition_count: usize,
    definitions: NumberMap<A>,
}

impl<A> DefinitionMap<A> {
    fn into_vec(self) -> Result<Vec<A>, IndexUndefinedError> {
        self.definitions
            .into_contiguous_values(self.definition_count)
    }

    pub(crate) fn new() -> Self {
        Self {
            definition_count: 0,
            definitions: NumberMap::new(0),
        }
    }

    pub(crate) fn reserve_definition(&mut self) -> usize {
        let result = self.definition_count;

        self.definition_count += 1;

        result
    }

    pub(crate) fn reserve_definitions(&mut self, count: usize) -> Range<usize> {
        let result = self.definition_count..self.definition_count + count;

        self.definition_count += count;

        result
    }

    pub(crate) fn set_definition(&mut self, i: usize, definition: A) {
        self.definitions.insert(i, definition);
    }
}

impl DefinitionMap<AnalyzedFunctionDefinition> {
    pub(crate) fn into_function_vec(
        self,
    ) -> Result<Vec<AnalyzedFunctionDefinition>, CompilationError> {
        self.into_vec().map_err(|error| CompilationError {
            message: CompilationErrorMessage::InternalError(
                InternalError::AnalyzerFunctionNotDefined { index: error.index },
            ),

            position: None,
        })
    }
}

impl DefinitionMap<AnalyzedStructDefinition> {
    pub(crate) fn into_struct_vec(self) -> Result<Vec<AnalyzedStructDefinition>, CompilationError> {
        self.into_vec().map_err(|error| CompilationError {
            message: CompilationErrorMessage::InternalError(
                InternalError::AnalyzerFunctionNotDefined { index: error.index },
            ),

            position: None,
        })
    }
}
