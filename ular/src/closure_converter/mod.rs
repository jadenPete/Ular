use crate::{
    arguments::PhaseName,
    data_structures::{
        cache::{Cache, FlexibleBorrow, FlexibleToOwned},
        graph::DirectedGraph,
    },
    dependency_analyzer::{
        analyzed_program::{
            AnalyzedBlock, AnalyzedCall, AnalyzedClosure, AnalyzedExpression,
            AnalyzedExpressionGraphRef, AnalyzedExpressionRef, AnalyzedFunctionDefinition,
            AnalyzedIf, AnalyzedInfixOperation, AnalyzedParameter, AnalyzedPrefixOperation,
            AnalyzedProgram, AnalyzedSelect, AnalyzedStructApplication,
            AnalyzedStructApplicationField, AnalyzedStructDefinition,
            AnalyzedStructDefinitionField, AnalyzedType, AnalyzerTyped,
        },
        DefinitionMap,
    },
    error_reporting::{CompilationError, CompilationErrorMessage, InternalError, Position},
    parser::program::{Identifier, Node},
    phase::built_in_values::{BuiltInPath, BuiltInPathBuf},
    Phase,
};
use hashbrown::Equivalent;
use std::collections::LinkedList;

struct ClosureConverter {
    function_closure_context_struct_index: Option<usize>,
    function_closure_functions: Cache<FunctionClosureFunctionsKeyBuf, usize>,
    functions: DefinitionMap<AnalyzedFunctionDefinition>,
    next_closure_index: usize,
    structs: DefinitionMap<AnalyzedStructDefinition>,
}

impl ClosureConverter {
    fn convert_block(
        &mut self,
        block: &AnalyzedBlock,
        offset_override: Option<usize>,
    ) -> Result<AnalyzedBlock, CompilationError> {
        let mut converted_expression_graph =
            self.convert_expression_graph(&block.expression_graph, offset_override)?;

        let converted_result = match &block.result {
            Some(result) => Some(
                self.convert_expression_ref(&mut converted_expression_graph, result)?
                    .expression,
            ),

            None => None,
        };

        Ok(AnalyzedBlock {
            expression_graph: converted_expression_graph,
            result: converted_result,
            position: block.get_position(),
        })
    }

    fn convert_call(
        &mut self,
        expression_graph: &mut DirectedGraph<AnalyzedExpression>,
        call: &AnalyzedCall,
    ) -> ConversionResult<AnalyzedCall> {
        let converted_function = self.convert_expression_ref(expression_graph, &call.function)?;
        let converted_arguments = call
            .arguments
            .iter()
            .map(|argument| self.convert_expression_ref(expression_graph, argument))
            .collect::<ConversionResult<_>>()?;

        Ok(converted_function.and_then(|function| {
            converted_arguments.map(|arguments| AnalyzedCall {
                function,
                arguments,
                type_: call.get_type(),
                position: call.get_position(),
            })
        }))
    }

    fn convert_closure(
        &mut self,
        closure: &AnalyzedClosure,
    ) -> ConversionResult<AnalyzedExpression> {
        let closure_index = self.next_closure_index;

        self.next_closure_index += 1;

        let context_struct_index = self.structs.reserve_definition();

        self.structs.set_definition(
            context_struct_index,
            AnalyzedStructDefinition {
                name: Identifier {
                    value: format!("_ClosureContext{}", closure_index),
                    position: Position::empty(),
                },

                fields: std::iter::once(AnalyzedStructDefinitionField {
                    name: Identifier {
                        value: "function".to_owned(),
                        position: Position::empty(),
                    },

                    type_: AnalyzedType::RawFunction,
                    position: Position::empty(),
                })
                .chain(
                    closure
                        .dependencies
                        .iter()
                        .enumerate()
                        .map(|(i, dependency)| AnalyzedStructDefinitionField {
                            name: Identifier {
                                value: format!("dependency{}", i),
                                position: Position::empty(),
                            },

                            type_: dependency.get_type(),
                            position: Position::empty(),
                        }),
                )
                .collect(),

                methods: Vec::new(),
                position: Position::empty(),
            },
        );

        let function_index = self.functions.reserve_definition();
        let function_parameters = closure
            .parameters
            .iter()
            .cloned()
            .chain(std::iter::once(AnalyzedParameter {
                name: "context".to_owned(),
                type_: AnalyzedType::Struct(context_struct_index),
                position: Position::empty(),
            }))
            .collect::<Vec<_>>();

        // Override the offset of the closure's expression graph so we can define nodes for each of
        // the closure's dependencies
        let mut function_body = self.convert_block(&closure.body, Some(0))?;

        for (i, dependency) in closure.dependencies.iter().enumerate() {
            function_body.expression_graph.set_node(
                dependency.index,
                AnalyzedExpression::Select(AnalyzedSelect {
                    left_hand_side: AnalyzedExpressionRef::Parameter {
                        index: function_parameters.len() - 1,
                        type_: AnalyzedType::Struct(context_struct_index),
                        position: Position::empty(),
                    },

                    field_index: i + 1,
                    type_: dependency.get_type(),
                    position: Position::empty(),
                }),
            );
        }

        // This is very hacky, but I can't think of a better way to do this.
        //
        // Consider the following program:
        // ```ular
        // message = "Hello, world!";
        //
        // say_hello = fn () {
        //     println_str(message);
        // };
        //
        // say_hello();
        // ```
        //
        // The analyzer phase will assign each expression an index, which may look like this:
        // ```ular
        // message = "Hello, world!"; # 0
        //
        // say_hello = fn () { # 1
        //     println_str(message); # 2
        // };
        //
        // say_hello(); # 2
        // ```
        //
        // Within the top-level scope, the expression graph would look like this:
        // - 1 -> 0
        // - 2 -> 1
        //
        // Within `say_hello`'s scope, the expression graph would have no edges.
        //
        // Then, this phase will transform the program into another like this:
        // ```ular
        // struct _ClosureContext0 {
        //     function: raw_function;
        //     dependency0: str;
        // }
        //
        // struct _FunctionClosureContext {
        //     function: raw_function;
        // }
        //
        // message = "Hello, world!"; # 0
        //
        // fn _closure_0(context: _ClosureContext0) {
        //     message = context.dependency0; # 0
        //
        //     _FunctionClosureContext {
        //         function: _function_closure_function_0,
        //     }(message); # 2
        // }
        //
        // fn _function_closure_function_0(context: _FunctionClosureContext, string: str) {
        //     println_str(string); # 0
        // }
        //
        // _ClosureContext0 {
        //     function: _closure_0,
        //     message: message,
        // } # 1
        // (); # 2
        // ```
        //
        // whose expression graphs would look like this:
        //
        // - Top-level:
        //   - 2 -> 1
        //   - 1 -> 0
        // - `_closure_0`:
        //   - (Empty)
        // - `_function_closure_function_0`:
        //   - (Empty)
        //
        // Can you spot the problem? Within `_closure_0`, there should be a dependency between nodes
        // 2 and 0, but there isn't because 0 wasn't a node in the expression graph when `_closure_0`
        // was still a closure.
        //
        // Our solution is to create a chain of dependencies from the closure's context dependencies,
        // and then have every node in the closure depend on the top node in that chain. This is
        // easier than trying to figure out which nodes in the closure depend on the closure's context
        // dependencies.
        if let Some(last_dependency) = closure.dependencies.last() {
            let closure_body_offset = closure.body.expression_graph.get_offset();
            let closure_body_nodes = function_body
                .expression_graph
                .node_iter()
                .filter(|&(i, _)| i >= closure_body_offset)
                .map(|(i, _)| i)
                .collect::<Vec<_>>();

            for i in closure_body_nodes {
                function_body
                    .expression_graph
                    .add_edge(i, last_dependency.index);
            }

            for i in 0..closure.dependencies.len() - 1 {
                function_body.expression_graph.add_edge(
                    closure.dependencies[i + 1].index,
                    closure.dependencies[i].index,
                );
            }
        }

        self.functions.set_definition(
            function_index,
            AnalyzedFunctionDefinition {
                name: Identifier {
                    value: format!("_closure_{}", closure_index),
                    position: Position::empty(),
                },

                parameters: function_parameters,
                body: function_body,
                type_: closure.type_.clone(),
                position: closure.get_position(),
            },
        );

        Ok(ConvertedExpression::without_added_dependencies(
            AnalyzedExpression::StructApplication(AnalyzedStructApplication {
                struct_index: context_struct_index,
                fields: std::iter::once(AnalyzedStructApplicationField {
                    name: "function".to_owned(),
                    value: AnalyzedExpressionRef::Function {
                        index: function_index,
                        type_: AnalyzedType::RawFunction,
                        position: Position::empty(),
                    },

                    position: Position::empty(),
                })
                .chain(
                    closure
                        .dependencies
                        .iter()
                        .enumerate()
                        .map(|(i, dependency)| AnalyzedStructApplicationField {
                            name: format!("dependency{}", i),
                            value: AnalyzedExpressionRef::Expression(dependency.clone()),
                            position: dependency.get_position(),
                        }),
                )
                .collect(),

                type_: closure.get_type(),
                position: closure.get_position(),
            }),
        ))
    }

    fn convert_expression(
        &mut self,
        expression_graph: &mut DirectedGraph<AnalyzedExpression>,
        expression: &AnalyzedExpression,
    ) -> ConversionResult<AnalyzedExpression> {
        Ok(match expression {
            AnalyzedExpression::If(if_expression) => self
                .convert_if(expression_graph, if_expression)?
                .map(AnalyzedExpression::If),

            AnalyzedExpression::InfixOperation(infix) => self
                .convert_infix_operation(expression_graph, infix)?
                .map(AnalyzedExpression::InfixOperation),

            AnalyzedExpression::Select(select) => self
                .convert_select(expression_graph, select)?
                .map(AnalyzedExpression::Select),

            AnalyzedExpression::Call(call) => self
                .convert_call(expression_graph, call)?
                .map(AnalyzedExpression::Call),

            AnalyzedExpression::Closure(closure) => self.convert_closure(closure)?,
            AnalyzedExpression::StructApplication(struct_application) => self
                .convert_struct_application(expression_graph, struct_application)?
                .map(AnalyzedExpression::StructApplication),

            AnalyzedExpression::PrefixOperation(prefix_operation) => self
                .convert_prefix_operation(expression_graph, prefix_operation)?
                .map(AnalyzedExpression::PrefixOperation),

            AnalyzedExpression::String(string) => ConvertedExpression::without_added_dependencies(
                AnalyzedExpression::String(string.clone()),
            ),
        })
    }

    fn convert_expression_graph(
        &mut self,
        graph: &DirectedGraph<AnalyzedExpression>,
        offset_override: Option<usize>,
    ) -> Result<DirectedGraph<AnalyzedExpression>, CompilationError> {
        let mut result = match offset_override {
            Some(offset) => {
                let mut result = DirectedGraph::new(offset);

                // Ensure any nodes we add don't overwrite those already there
                result.reserve_nodes(graph.get_offset() + graph.number_reserved() - offset);
                result
            }

            None => {
                let mut result = DirectedGraph::new(graph.get_offset());

                // Ensure any nodes we add don't overwrite those already there
                result.reserve_nodes(graph.number_reserved());
                result
            }
        };

        for (i, node) in graph.node_iter() {
            let converted_node = self.convert_expression(&mut result, node)?;

            result.set_node(i, converted_node.expression);

            for added_dependency in converted_node.added_dependencies {
                result.add_edge(i, added_dependency);
            }
        }

        for (i, j) in graph.edge_iter() {
            result.add_edge(i, j);
        }

        Ok(result)
    }

    fn convert_expression_ref(
        &mut self,
        expression_graph: &mut DirectedGraph<AnalyzedExpression>,
        reference: &AnalyzedExpressionRef,
    ) -> ConversionResult<AnalyzedExpressionRef> {
        let (function_with_raw_type, key) = match reference {
            AnalyzedExpressionRef::BuiltIn {
                path,
                position,
                type_: AnalyzedType::Function(_),
            } => (
                AnalyzedExpressionRef::BuiltIn {
                    path: path.clone(),
                    type_: AnalyzedType::RawFunction,
                    position: position.clone(),
                },
                FunctionClosureFunctionsKey::BuiltIn(path.flexible_borrow()),
            ),

            AnalyzedExpressionRef::Function {
                index, position, ..
            } => (
                AnalyzedExpressionRef::Function {
                    index: *index,
                    type_: AnalyzedType::RawFunction,
                    position: position.clone(),
                },
                FunctionClosureFunctionsKey::Function { index: *index },
            ),

            AnalyzedExpressionRef::StructMethod {
                struct_index,
                method_index,
                position,
                ..
            } => (
                AnalyzedExpressionRef::StructMethod {
                    struct_index: *struct_index,
                    method_index: *method_index,
                    type_: AnalyzedType::RawFunction,
                    position: position.clone(),
                },
                FunctionClosureFunctionsKey::StructMethod {
                    struct_index: *struct_index,
                    method_index: *method_index,
                },
            ),

            _ => {
                return Ok(ConvertedExpression::without_added_dependencies(
                    reference.clone(),
                ))
            }
        };

        let context_struct_index = *self
            .function_closure_context_struct_index
            .get_or_insert_with(|| {
                let i = self.structs.reserve_definition();

                self.structs.set_definition(
                    i,
                    AnalyzedStructDefinition {
                        name: Identifier {
                            value: "_FunctionClosureContext".to_owned(),
                            position: Position::empty(),
                        },

                        fields: vec![AnalyzedStructDefinitionField {
                            name: Identifier {
                                value: "function".to_owned(),
                                position: Position::empty(),
                            },

                            type_: AnalyzedType::RawFunction,
                            position: Position::empty(),
                        }],

                        methods: Vec::new(),
                        position: Position::empty(),
                    },
                );

                i
            });

        let function_type = reference.get_type();
        let function_function_type = if let AnalyzedType::Function(type_) = &function_type {
            type_
        } else {
            return Err(CompilationError {
                message: CompilationErrorMessage::InternalError(
                    InternalError::ClosureConverterExpectedFunctionType {
                        actual_type: format!("{:?}", function_type),
                    },
                ),

                position: Some(reference.get_position()),
            });
        };

        let wrapper_function_index = self.function_closure_functions.get_or_compute(&key, || {
            let return_type = function_function_type.return_type.as_ref().clone();
            let wrapper_function_index = self.functions.reserve_definition();
            let mut wrapper_function_expression_graph = DirectedGraph::new(0);

            wrapper_function_expression_graph.set_node(
                0,
                AnalyzedExpression::Call(AnalyzedCall {
                    function: function_with_raw_type,
                    arguments: function_function_type
                        .parameters
                        .iter()
                        .enumerate()
                        .map(|(i, parameter_type)| AnalyzedExpressionRef::Parameter {
                            index: i,
                            type_: parameter_type.clone(),
                            position: Position::empty(),
                        })
                        .collect(),

                    type_: return_type.clone(),
                    position: Position::empty(),
                }),
            );

            self.functions.set_definition(
                wrapper_function_index,
                AnalyzedFunctionDefinition {
                    name: Identifier {
                        value: format!(
                            "_function_closure_function_{}",
                            self.function_closure_functions.len()
                        ),

                        position: Position::empty(),
                    },

                    parameters: function_function_type
                        .parameters
                        .iter()
                        .enumerate()
                        .map(|(i, parameter_type)| AnalyzedParameter {
                            name: format!("parameter_{}", i),
                            type_: parameter_type.clone(),
                            position: Position::empty(),
                        })
                        .chain(std::iter::once(AnalyzedParameter {
                            name: "context".to_owned(),
                            type_: AnalyzedType::Struct(context_struct_index),
                            position: Position::empty(),
                        }))
                        .collect(),

                    body: AnalyzedBlock {
                        expression_graph: wrapper_function_expression_graph,
                        result: Some(AnalyzedExpressionRef::Expression(
                            AnalyzedExpressionGraphRef {
                                index: 0,
                                type_: return_type,
                                position: Position::empty(),
                            },
                        )),

                        position: Position::empty(),
                    },

                    type_: function_function_type.clone(),
                    position: Position::empty(),
                },
            );

            wrapper_function_index
        });

        let wrapper_function_reference = AnalyzedExpressionRef::Function {
            index: wrapper_function_index,
            type_: AnalyzedType::RawFunction,
            position: Position::empty(),
        };

        let function_position = reference.get_position();
        let (i, _) = expression_graph.add_node(AnalyzedExpression::StructApplication(
            AnalyzedStructApplication {
                struct_index: context_struct_index,
                fields: vec![AnalyzedStructApplicationField {
                    name: "function".to_owned(),
                    value: wrapper_function_reference,
                    position: function_position.clone(),
                }],

                type_: function_type.clone(),
                position: function_position.clone(),
            },
        ));

        Ok(ConvertedExpression {
            expression: AnalyzedExpressionRef::Expression(AnalyzedExpressionGraphRef {
                index: i,
                type_: function_type,
                position: function_position,
            }),

            added_dependencies: LinkedList::from([i]),
        })
    }

    fn convert_if(
        &mut self,
        expression_graph: &mut DirectedGraph<AnalyzedExpression>,
        if_expression: &AnalyzedIf,
    ) -> ConversionResult<AnalyzedIf> {
        let converted_condition =
            self.convert_expression_ref(expression_graph, &if_expression.condition)?;

        let converted_then_block = self.convert_block(&if_expression.then_block, None)?;
        let converted_else_block = self.convert_block(&if_expression.else_block, None)?;

        Ok(converted_condition.map(|condition| AnalyzedIf {
            condition,
            then_block: converted_then_block,
            else_block: converted_else_block,
            type_: if_expression.get_type(),
            position: if_expression.get_position(),
        }))
    }

    fn convert_infix_operation(
        &mut self,
        expression_graph: &mut DirectedGraph<AnalyzedExpression>,
        infix_operation: &AnalyzedInfixOperation,
    ) -> ConversionResult<AnalyzedInfixOperation> {
        let converted_left =
            self.convert_expression_ref(expression_graph, &infix_operation.left)?;

        let converted_right =
            self.convert_expression_ref(expression_graph, &infix_operation.right)?;

        Ok(converted_left.and_then(|left| {
            converted_right.map(|right| AnalyzedInfixOperation {
                left,
                operator: infix_operation.operator,
                right,
                type_: infix_operation.get_type(),
                position: infix_operation.get_position(),
            })
        }))
    }

    fn convert_function_definition(
        &mut self,
        definition: &AnalyzedFunctionDefinition,
    ) -> Result<AnalyzedFunctionDefinition, CompilationError> {
        Ok(AnalyzedFunctionDefinition {
            name: definition.name.clone(),
            parameters: definition.parameters.clone(),
            body: self.convert_block(&definition.body, None)?,
            type_: definition.type_.clone(),
            position: definition.position.clone(),
        })
    }

    fn convert_prefix_operation(
        &mut self,
        expression_graph: &mut DirectedGraph<AnalyzedExpression>,
        prefix_operation: &AnalyzedPrefixOperation,
    ) -> ConversionResult<AnalyzedPrefixOperation> {
        Ok(self
            .convert_expression_ref(expression_graph, &prefix_operation.expression)?
            .map(|expression| AnalyzedPrefixOperation {
                operator: prefix_operation.operator,
                expression,
                type_: prefix_operation.get_type(),
                position: prefix_operation.get_position(),
            }))
    }

    fn convert_program(
        mut self,
        program: &AnalyzedProgram,
    ) -> Result<AnalyzedProgram, CompilationError> {
        self.functions.reserve_definitions(program.functions.len());
        self.structs.reserve_definitions(program.structs.len());

        for (i, definition) in program.structs.iter().enumerate() {
            let converted = self.convert_struct_definition(definition)?;

            self.structs.set_definition(i, converted);
        }

        for (i, definition) in program.functions.iter().enumerate() {
            let converted = self.convert_function_definition(definition)?;

            self.functions.set_definition(i, converted);
        }

        let expression_graph = self.convert_expression_graph(&program.expression_graph, None)?;

        Ok(AnalyzedProgram {
            string_literals: program.string_literals.clone(),
            structs: self.structs.into_struct_vec()?,
            functions: self.functions.into_function_vec()?,
            expression_graph,
            position: program.position.clone(),
        })
    }

    fn convert_select(
        &mut self,
        expression_graph: &mut DirectedGraph<AnalyzedExpression>,
        select: &AnalyzedSelect,
    ) -> ConversionResult<AnalyzedSelect> {
        Ok(self
            .convert_expression_ref(expression_graph, &select.left_hand_side)?
            .map(|left_hand_side| AnalyzedSelect {
                left_hand_side,
                field_index: select.field_index,
                type_: select.get_type(),
                position: select.get_position(),
            }))
    }

    fn convert_struct_application(
        &mut self,
        expression_graph: &mut DirectedGraph<AnalyzedExpression>,
        struct_application: &AnalyzedStructApplication,
    ) -> ConversionResult<AnalyzedStructApplication> {
        Ok(struct_application
            .fields
            .iter()
            .map(|field| {
                Ok(self
                    .convert_expression_ref(expression_graph, &field.value)?
                    .map(|value| AnalyzedStructApplicationField {
                        name: field.name.clone(),
                        value,
                        position: field.get_position(),
                    }))
            })
            .collect::<ConversionResult<_>>()?
            .map(|fields| AnalyzedStructApplication {
                struct_index: struct_application.struct_index,
                fields,
                type_: struct_application.get_type(),
                position: struct_application.get_position(),
            }))
    }

    fn convert_struct_definition(
        &mut self,
        definition: &AnalyzedStructDefinition,
    ) -> Result<AnalyzedStructDefinition, CompilationError> {
        Ok(AnalyzedStructDefinition {
            name: definition.name.clone(),
            fields: definition.fields.clone(),
            methods: definition
                .methods
                .iter()
                .map(|method| self.convert_function_definition(method))
                .collect::<Result<_, _>>()?,

            position: definition.position.clone(),
        })
    }

    fn new() -> Self {
        Self {
            function_closure_context_struct_index: None,
            function_closure_functions: Cache::new(),
            functions: DefinitionMap::new(),
            next_closure_index: 0,
            structs: DefinitionMap::new(),
        }
    }
}

struct ConvertedExpression<A> {
    expression: A,
    added_dependencies: LinkedList<usize>,
}

impl<A> ConvertedExpression<A> {
    fn and_then<B, Callback: FnOnce(A) -> ConvertedExpression<B>>(
        mut self,
        callback: Callback,
    ) -> ConvertedExpression<B> {
        let mut result = callback(self.expression);

        self.added_dependencies
            .append(&mut result.added_dependencies);

        ConvertedExpression {
            expression: result.expression,
            added_dependencies: self.added_dependencies,
        }
    }

    fn map<B, Callback: FnOnce(A) -> B>(self, callback: Callback) -> ConvertedExpression<B> {
        ConvertedExpression {
            expression: callback(self.expression),
            added_dependencies: self.added_dependencies,
        }
    }

    fn without_added_dependencies(expression: A) -> Self {
        Self {
            expression,
            added_dependencies: LinkedList::new(),
        }
    }
}

impl<Expression, Collection: Default + Extend<Expression>>
    FromIterator<ConvertedExpression<Expression>> for ConvertedExpression<Collection>
{
    fn from_iter<A: IntoIterator<Item = ConvertedExpression<Expression>>>(iterator: A) -> Self {
        let (expressions, added_dependencies): (Collection, Vec<LinkedList<usize>>) = iterator
            .into_iter()
            .map(|converted| (converted.expression, converted.added_dependencies))
            .unzip();

        Self {
            expression: expressions,
            added_dependencies: added_dependencies.into_iter().fold(
                LinkedList::new(),
                |mut result, mut dependencies| {
                    result.append(&mut dependencies);
                    result
                },
            ),
        }
    }
}

type ConversionResult<A> = Result<ConvertedExpression<A>, CompilationError>;

#[derive(Eq, Hash, PartialEq)]
enum FunctionClosureFunctionsKey<'a> {
    BuiltIn(BuiltInPath<'a>),
    Function {
        index: usize,
    },
    StructMethod {
        struct_index: usize,
        method_index: usize,
    },
}

impl Equivalent<FunctionClosureFunctionsKeyBuf> for FunctionClosureFunctionsKey<'_> {
    fn equivalent(&self, key: &FunctionClosureFunctionsKeyBuf) -> bool {
        self == &key.flexible_borrow()
    }
}

impl<'a> FlexibleToOwned<'a> for FunctionClosureFunctionsKey<'a> {
    type Owned = FunctionClosureFunctionsKeyBuf;

    fn to_flexible_owned(&'a self) -> Self::Owned {
        match self {
            Self::BuiltIn(path) => {
                FunctionClosureFunctionsKeyBuf::BuiltIn((*path).to_flexible_owned())
            }

            Self::Function { index } => FunctionClosureFunctionsKeyBuf::Function { index: *index },
            Self::StructMethod {
                struct_index,
                method_index,
            } => FunctionClosureFunctionsKeyBuf::StructMethod {
                struct_index: *struct_index,
                method_index: *method_index,
            },
        }
    }
}

#[derive(Eq, Hash, PartialEq)]
enum FunctionClosureFunctionsKeyBuf {
    BuiltIn(BuiltInPathBuf),
    Function {
        index: usize,
    },
    StructMethod {
        struct_index: usize,
        method_index: usize,
    },
}

impl<'a> FlexibleBorrow<'a, FunctionClosureFunctionsKey<'a>> for FunctionClosureFunctionsKeyBuf {
    fn flexible_borrow(&'a self) -> FunctionClosureFunctionsKey<'a> {
        match self {
            Self::BuiltIn(path) => FunctionClosureFunctionsKey::BuiltIn((*path).flexible_borrow()),
            Self::Function { index } => FunctionClosureFunctionsKey::Function { index: *index },
            Self::StructMethod {
                struct_index,
                method_index,
            } => FunctionClosureFunctionsKey::StructMethod {
                struct_index: *struct_index,
                method_index: *method_index,
            },
        }
    }
}

pub(crate) struct ClosureConverterPhase;

impl Phase<&AnalyzedProgram> for ClosureConverterPhase {
    type Output = AnalyzedProgram;

    fn execute(&self, program: &AnalyzedProgram) -> Result<Self::Output, CompilationError> {
        ClosureConverter::new().convert_program(program)
    }

    fn name() -> PhaseName {
        PhaseName::ClosureConverter
    }
}
