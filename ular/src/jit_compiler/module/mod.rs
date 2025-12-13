pub mod built_in_values;
pub(super) mod fork_function_cache;
pub(super) mod memory_manager;

use crate::{
    dependency_analyzer::analyzed_program::AnalyzedStructDefinition,
    mmtk::object_descriptor_store::ObjectDescriptorReference,
};
use inkwell::{
    module::{Linkage, Module},
    types::{BasicType, FunctionType, StructType},
    values::{FunctionValue, GlobalValue},
};

pub(super) struct StructInformation<'a, 'context> {
    pub(super) definition: &'a AnalyzedStructDefinition,
    pub(super) inkwell_type: StructType<'context>,
    pub(super) descriptor_reference: ObjectDescriptorReference,
}

pub struct UlarModule<'a> {
    pub(super) underlying: Module<'a>,
    next_global_value: u32,
}

impl<'a> UlarModule<'a> {
    pub(super) fn add_garbage_collecting_function(
        &self,
        name: &str,
        type_: FunctionType<'a>,
        linkage: Option<Linkage>,
    ) -> FunctionValue<'a> {
        let result = self.underlying.add_function(name, type_, linkage);

        result.set_gc("statepoint-example");
        result
    }

    pub(super) fn add_global<A: BasicType<'a>>(&mut self, type_: A) -> GlobalValue<'a> {
        let result = self
            .underlying
            .add_global(type_, None, &self.next_global_value.to_string());

        self.next_global_value += 1;

        result
    }
}

impl<'a> From<Module<'a>> for UlarModule<'a> {
    fn from(module: Module<'a>) -> Self {
        Self {
            underlying: module,
            next_global_value: 0,
        }
    }
}
