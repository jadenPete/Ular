use inkwell::{
    module::{Linkage, Module},
    types::{BasicType, FunctionType},
    values::{FunctionValue, GlobalValue},
};

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
