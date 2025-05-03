use crate::jit_compiler::{built_in_values::BuiltInValues, module::UlarModule, value::UlarValue};
use inkwell::{
    basic_block::BasicBlock, builder::Builder, context::Context, execution_engine::ExecutionEngine,
};

use std::cmp::Eq;
use std::collections::HashMap;

pub struct JitCompilerScope<'a, 'context> {
    parent: Option<&'a JitCompilerScope<'a, 'context>>,
    pub basic_block: BasicBlock<'context>,
    next_local_name: LocalName,
    variable_values: HashMap<String, UlarValue<'context>>,
}

impl<'a, 'context> JitCompilerScope<'a, 'context> {
    pub fn new(
        basic_block: BasicBlock<'context>,
        parent: Option<&'a JitCompilerScope<'a, 'context>>,
    ) -> Self {
        let next_local_name = match parent {
            Some(parent) => parent.next_local_name,
            None => LocalName(0),
        };

        Self {
            parent,
            basic_block,
            next_local_name,
            variable_values: HashMap::new(),
        }
    }

    pub fn declare_variable(&mut self, name: String, value: UlarValue<'context>) -> bool {
        let result = self.variable_values.contains_key(&name);

        if !result {
            self.variable_values.insert(name, value);
        }

        result
    }

    pub fn get_local_name(&mut self) -> LocalName {
        let result = self.next_local_name;

        self.next_local_name = LocalName(self.next_local_name.0 + 1);

        result
    }

    pub fn get_variable_value(
        &self,
        variable_name: &str,
        local_name: LocalName,
        context: &'context Context,
        builder: &Builder<'context>,
        built_in_values: &mut BuiltInValues<'context>,
        execution_engine: &ExecutionEngine<'context>,
        module: &mut UlarModule<'context>,
    ) -> Option<UlarValue<'context>> {
        self.variable_values
            .get(variable_name)
            .copied()
            .or_else(|| {
                self.parent.and_then(|parent| {
                    parent.get_variable_value(
                        variable_name,
                        local_name,
                        context,
                        builder,
                        built_in_values,
                        execution_engine,
                        module,
                    )
                })
            })
            .or_else(|| {
                built_in_values.get(
                    variable_name,
                    local_name,
                    context,
                    builder,
                    execution_engine,
                    module,
                )
            })
    }

    pub fn has_parent(&self) -> bool {
        self.parent.is_some()
    }

    pub fn with_child<A: FnOnce(&mut JitCompilerScope<'_, 'context>) -> B, B>(
        &mut self,
        basic_block: BasicBlock<'context>,
        callback: A,
    ) -> B {
        let mut child = JitCompilerScope::new(basic_block, Some(self));
        let result = callback(&mut child);

        self.next_local_name = child.next_local_name;

        result
    }
}

#[derive(Clone, Copy, Eq, Hash, PartialEq)]
pub struct LocalName(u32);

impl ToString for LocalName {
    fn to_string(&self) -> String {
        self.0.to_string()
    }
}
