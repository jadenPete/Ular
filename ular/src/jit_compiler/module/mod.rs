pub mod built_in_values;
pub(super) mod fork_function_cache;
pub(super) mod global_value_registry;
pub(super) mod memory_manager;

use crate::{
    dependency_analyzer::analyzed_program::AnalyzedStructDefinition,
    mmtk::object_descriptor_store::ObjectDescriptorReference,
};
use inkwell::{
    module::{Linkage, Module},
    types::{FunctionType, StructType},
    values::FunctionValue,
};

pub(super) struct StructInformation<'a, 'context> {
    pub(super) definition: &'a AnalyzedStructDefinition,
    pub(super) inkwell_type: StructType<'context>,
    pub(super) descriptor_reference: ObjectDescriptorReference,
}

pub(super) fn add_garbage_collecting_function<'a>(
    module: &Module<'a>,
    name: &str,
    type_: FunctionType<'a>,
    linkage: Option<Linkage>,
) -> FunctionValue<'a> {
    let result = module.add_function(name, type_, linkage);

    result.set_gc("statepoint-example");
    result
}
