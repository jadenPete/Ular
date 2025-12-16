use inkwell::{module::Module, types::BasicType, values::GlobalValue, AddressSpace};
use std::cell::Cell;

pub(in crate::jit_compiler) struct GlobalValueRegistry<'a, 'context> {
    module: &'a Module<'context>,
    next_global_value: Cell<u32>,
}

impl<'a, 'context> GlobalValueRegistry<'a, 'context> {
    pub(in crate::jit_compiler) fn add_global<A: BasicType<'context>>(
        &self,
        type_: A,
        address_space: Option<AddressSpace>,
    ) -> GlobalValue<'context> {
        let name = self.next_global_value.get();

        self.next_global_value.set(self.next_global_value.get() + 1);

        let result = self
            .module
            .add_global(type_, address_space, &name.to_string());

        result
    }

    pub(in crate::jit_compiler) fn new(module: &'a Module<'context>) -> Self {
        Self {
            module,
            next_global_value: Cell::new(0),
        }
    }
}
