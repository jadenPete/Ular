use inkwell::{module::Module, types::BasicType, values::GlobalValue};

pub struct UlarModule<'a> {
    pub underlying: Module<'a>,
    next_global_value: u32,
}

impl<'a> UlarModule<'a> {
    pub fn add_global<A: BasicType<'a>>(&mut self, type_: A) -> GlobalValue<'a> {
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
