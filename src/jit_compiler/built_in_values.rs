use crate::jit_compiler::{module::UlarModule, scope::LocalName, value::UlarValue};
use inkwell::{
    builder::Builder, context::Context, execution_engine::ExecutionEngine, types::FunctionType,
    values::BasicValue, AddressSpace, GlobalVisibility,
};

use std::collections::HashMap;

use super::value::UlarFunction;

trait BuiltInValue<'a> {
    fn get_value(
        &mut self,
        local_name: LocalName,
        context: &'a Context,
        builder: &Builder<'a>,
        execution_engine: &ExecutionEngine<'a>,
        module: &mut UlarModule<'a>,
    ) -> UlarValue<'a>;
}

struct BuiltInBoolean<'a> {
    value: u64,
    computed_value: Option<UlarValue<'a>>,
}

impl<'a> BuiltInBoolean<'a> {
    fn new(value: u64) -> Self {
        Self {
            value,
            computed_value: None,
        }
    }
}

impl<'a> BuiltInValue<'a> for BuiltInBoolean<'a> {
    fn get_value(
        &mut self,
        local_name: LocalName,
        context: &'a Context,
        builder: &Builder<'a>,
        _execution_engine: &ExecutionEngine,
        module: &mut UlarModule<'a>,
    ) -> UlarValue<'a> {
        self.computed_value.unwrap_or_else(|| {
            let global = module.add_global(context.i8_type());

            global.set_visibility(GlobalVisibility::Hidden);
            global.set_constant(true);
            global.set_initializer(
                &context
                    .i8_type()
                    .const_int(self.value, false)
                    .as_basic_value_enum(),
            );

            let result = builder
                .build_load(
                    context.i8_type(),
                    global.as_pointer_value(),
                    &local_name.to_string(),
                )
                .unwrap()
                .into_int_value()
                .into();

            self.computed_value = Some(result);

            result
        })
    }
}

struct BuiltInFunction<'a> {
    name: String,
    address: usize,
    signature: FunctionType<'a>,
    value: Option<UlarValue<'a>>,
}

impl<'a> BuiltInFunction<'a> {
    fn new(name: String, address: usize, signature: FunctionType<'a>) -> Self {
        Self {
            name,
            address,
            signature,
            value: None,
        }
    }
}

impl<'a> BuiltInValue<'a> for BuiltInFunction<'a> {
    fn get_value(
        &mut self,
        local_name: LocalName,
        context: &'a Context,
        builder: &Builder<'a>,
        execution_engine: &ExecutionEngine<'a>,
        module: &mut UlarModule<'a>,
    ) -> UlarValue<'a> {
        self.value.unwrap_or_else(|| {
            let function = module
                .underlying
                .add_function(&self.name, self.signature, None);

            execution_engine.add_global_mapping(&function, self.address);

            let value = UlarValue::Function(UlarFunction {
                pointer: builder
                    .build_bit_cast(
                        function.as_global_value().as_pointer_value(),
                        context.ptr_type(AddressSpace::default()),
                        &local_name.to_string(),
                    )
                    .unwrap()
                    .into_pointer_value(),
                type_: self.signature,
            });

            self.value = Some(value);

            value
        })
    }
}

pub struct BuiltInValues<'a> {
    values: HashMap<String, Box<dyn BuiltInValue<'a> + 'a>>,
}

impl<'a> BuiltInValues<'a> {
    pub fn get(
        &mut self,
        variable_name: &str,
        local_name: LocalName,
        context: &'a Context,
        builder: &Builder<'a>,
        execution_engine: &ExecutionEngine<'a>,
        module: &mut UlarModule<'a>,
    ) -> Option<UlarValue<'a>> {
        Some(self.values.get_mut(variable_name)?.get_value(
            local_name,
            context,
            builder,
            execution_engine,
            module,
        ))
    }

    pub fn new(context: &'a Context) -> Self {
        let mut values = HashMap::<String, Box<dyn BuiltInValue>>::new();

        values.insert(String::from("true"), Box::new(BuiltInBoolean::new(1)));
        values.insert(String::from("false"), Box::new(BuiltInBoolean::new(0)));
        values.insert(
            String::from("println_bool"),
            Box::new(BuiltInFunction::new(
                String::from("println_bool"),
                println_bool as usize,
                context
                    .void_type()
                    .fn_type(&[context.i8_type().into()], false),
            )),
        );

        values.insert(
            String::from("println_number"),
            Box::new(BuiltInFunction::new(
                String::from("println_number"),
                println_number as usize,
                context
                    .void_type()
                    .fn_type(&[context.i32_type().into()], false),
            )),
        );

        Self { values }
    }
}

pub extern "C" fn println_bool(value: u8) {
    println!("{}", value == 1);
}

pub extern "C" fn println_number(value: i32) {
    println!("{}", value);
}
