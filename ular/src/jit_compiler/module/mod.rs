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
use std::cell::Cell;

pub(super) struct StructInformation<'a, 'context> {
    pub(super) definition: &'a AnalyzedStructDefinition,
    pub(super) inkwell_type: StructType<'context>,
    pub(super) descriptor_reference: ObjectDescriptorReference,
}

pub struct UlarModule<'a> {
    pub(super) underlying: Module<'a>,
    next_global_value: Cell<u32>,
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

    pub(super) fn add_global<A: BasicType<'a>>(&self, type_: A) -> GlobalValue<'a> {
        let name = self.next_global_value.get();

        self.next_global_value.set(self.next_global_value.get() + 1);

        let result = self.underlying.add_global(type_, None, &name.to_string());

        result
    }
}

impl<'a> From<Module<'a>> for UlarModule<'a> {
    fn from(module: Module<'a>) -> Self {
        Self {
            underlying: module,
            next_global_value: Cell::new(0),
        }
    }
}
