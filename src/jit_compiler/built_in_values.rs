use crate::jit_compiler::{
    module::UlarModule,
    scope::LocalName,
    value::{UlarFunction, UlarValue},
};

use inkwell::{
    builder::Builder,
    context::Context,
    execution_engine::ExecutionEngine,
    module::Linkage,
    types::FunctionType,
    values::{BasicValue, FunctionValue},
    AddressSpace, GlobalVisibility,
};

use std::{
    collections::HashMap,
    ffi::{c_char, CString},
    ptr::null,
};

extern "C" {
    fn __cxa_allocate_exception(size: usize) -> *mut *mut c_char;
    fn __cxa_throw(
        exception_object: *mut *mut c_char,
        type_info: *const i8,
        destructor: Option<extern "C" fn(*mut *mut c_char)>,
    ) -> !;
}

pub trait BuiltInValue<'a> {
    fn get_value(
        &mut self,
        local_name: LocalName,
        context: &'a Context,
        builder: &Builder<'a>,
        execution_engine: &ExecutionEngine<'a>,
        module: &mut UlarModule<'a>,
    ) -> UlarValue<'a>;
}

pub struct BuiltInBoolean<'a> {
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

pub trait BuiltInFunction<'a> {
    fn build_function(
        &self,
        context: &'a Context,
        execution_engine: &ExecutionEngine<'a>,
        module: &mut UlarModule<'a>,
    ) -> FunctionValue<'a>;

    fn get_stored_function(&self) -> Option<FunctionValue<'a>>;
    fn set_stored_function(&mut self, function: FunctionValue<'a>);

    fn get_function(
        &mut self,
        local_name: LocalName,
        context: &'a Context,
        builder: &Builder<'a>,
        execution_engine: &ExecutionEngine<'a>,
        module: &mut UlarModule<'a>,
    ) -> UlarFunction<'a> {
        let inkwell_function = self.get_inkwell_function(context, execution_engine, module);

        UlarFunction {
            pointer: builder
                .build_bit_cast(
                    inkwell_function.as_global_value().as_pointer_value(),
                    context.ptr_type(AddressSpace::default()),
                    &local_name.to_string(),
                )
                .unwrap()
                .into_pointer_value(),
            type_: inkwell_function.get_type(),
        }
    }

    fn get_inkwell_function(
        &mut self,
        context: &'a Context,
        execution_engine: &ExecutionEngine<'a>,
        module: &mut UlarModule<'a>,
    ) -> FunctionValue<'a> {
        self.get_stored_function().unwrap_or_else(|| {
            let function = self.build_function(context, execution_engine, module);

            self.set_stored_function(function);

            function
        })
    }
}

impl<'a, A: BuiltInFunction<'a>> BuiltInValue<'a> for A {
    fn get_value(
        &mut self,
        local_name: LocalName,
        context: &'a Context,
        builder: &Builder<'a>,
        execution_engine: &ExecutionEngine<'a>,
        module: &mut UlarModule<'a>,
    ) -> UlarValue<'a> {
        UlarValue::Function(self.get_function(
            local_name,
            context,
            builder,
            execution_engine,
            module,
        ))
    }
}

pub struct BuiltInLinkedFunction<'a> {
    name: String,
    signature: FunctionType<'a>,
    stored_function: Option<FunctionValue<'a>>,
}

impl<'a> BuiltInLinkedFunction<'a> {
    fn new(name: String, signature: FunctionType<'a>) -> Self {
        Self {
            name,
            signature,
            stored_function: None,
        }
    }
}

impl<'a> BuiltInFunction<'a> for BuiltInLinkedFunction<'a> {
    fn build_function(
        &self,
        _context: &'a Context,
        _execution_engine: &ExecutionEngine<'a>,
        module: &mut UlarModule<'a>,
    ) -> FunctionValue<'a> {
        module
            .underlying
            .add_function(&self.name, self.signature, Some(Linkage::External))
    }

    fn get_stored_function(&self) -> Option<FunctionValue<'a>> {
        self.stored_function
    }

    fn set_stored_function(&mut self, function: FunctionValue<'a>) {
        let _ = self.stored_function.insert(function);
    }
}

pub struct BuiltInMappedFunction<'a> {
    name: String,
    signature: FunctionType<'a>,
    address: usize,
    stored_function: Option<FunctionValue<'a>>,
}

impl<'a> BuiltInMappedFunction<'a> {
    fn new(name: String, signature: FunctionType<'a>, address: usize) -> Self {
        Self {
            name,
            signature,
            address,
            stored_function: None,
        }
    }
}

impl<'a> BuiltInFunction<'a> for BuiltInMappedFunction<'a> {
    fn build_function(
        &self,
        _context: &'a Context,
        execution_engine: &ExecutionEngine<'a>,
        module: &mut UlarModule<'a>,
    ) -> FunctionValue<'a> {
        let result = module
            .underlying
            .add_function(&self.name, self.signature, None);

        execution_engine.add_global_mapping(&result, self.address);

        result
    }

    fn get_stored_function(&self) -> Option<FunctionValue<'a>> {
        self.stored_function
    }

    fn set_stored_function(&mut self, function: FunctionValue<'a>) {
        let _ = self.stored_function.insert(function);
    }
}

pub struct BuiltInValues<'a> {
    referencable_values: HashMap<String, Box<dyn BuiltInValue<'a> + 'a>>,
    pub __cxa_allocate_exception: BuiltInLinkedFunction<'a>,
    pub __cxa_begin_catch: BuiltInLinkedFunction<'a>,
    pub __cxa_end_catch: BuiltInLinkedFunction<'a>,
    pub __cxa_throw: BuiltInLinkedFunction<'a>,
    pub __gxx_personality_v0: BuiltInLinkedFunction<'a>,
    pub _divide_number: BuiltInMappedFunction<'a>,
    pub _print_c_string: BuiltInMappedFunction<'a>,
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
        Some(self.referencable_values.get_mut(variable_name)?.get_value(
            local_name,
            context,
            builder,
            execution_engine,
            module,
        ))
    }

    pub fn new(context: &'a Context) -> Self {
        let mut referencable_values = HashMap::<String, Box<dyn BuiltInValue>>::new();

        referencable_values.insert(String::from("true"), Box::new(BuiltInBoolean::new(1)));
        referencable_values.insert(String::from("false"), Box::new(BuiltInBoolean::new(0)));
        referencable_values.insert(
            String::from("println_bool"),
            Box::new(BuiltInMappedFunction::new(
                String::from("println_bool"),
                context
                    .void_type()
                    .fn_type(&[context.i8_type().into()], false),
                println_bool as usize,
            )),
        );

        referencable_values.insert(
            String::from("println_number"),
            Box::new(BuiltInMappedFunction::new(
                String::from("println_number"),
                context
                    .void_type()
                    .fn_type(&[context.i32_type().into()], false),
                println_number as usize,
            )),
        );

        let __cxa_allocate_exception = BuiltInLinkedFunction::new(
            String::from("__cxa_allocate_exception"),
            context
                .ptr_type(AddressSpace::default())
                .fn_type(&[context.i64_type().into()], false),
        );

        let __cxa_begin_catch = BuiltInLinkedFunction::new(
            String::from("__cxa_begin_catch"),
            context
                .ptr_type(AddressSpace::default())
                .fn_type(&[context.ptr_type(AddressSpace::default()).into()], false),
        );

        let __cxa_end_catch = BuiltInLinkedFunction::new(
            String::from("__cxa_end_catch"),
            context.void_type().fn_type(&[], false),
        );

        let __cxa_throw = BuiltInLinkedFunction::new(
            String::from("__cxa_throw"),
            context.void_type().fn_type(
                &[
                    context.ptr_type(AddressSpace::default()).into(),
                    context.ptr_type(AddressSpace::default()).into(),
                    context.ptr_type(AddressSpace::default()).into(),
                ],
                false,
            ),
        );

        let __gxx_personality_v0 = BuiltInLinkedFunction::new(
            String::from("__gxx_personality_v0"),
            context.i32_type().fn_type(&[], true),
        );

        let _divide_number = BuiltInMappedFunction::new(
            String::from("_divide_number"),
            context.i32_type().fn_type(
                &[context.i32_type().into(), context.i32_type().into()],
                false,
            ),
            _divide_number as usize,
        );

        let _print_c_string = BuiltInMappedFunction::new(
            String::from("_print_c_string"),
            context
                .void_type()
                .fn_type(&[context.ptr_type(AddressSpace::default()).into()], false),
            _print_c_string as usize,
        );

        Self {
            referencable_values,
            __cxa_allocate_exception,
            __cxa_begin_catch,
            __cxa_end_catch,
            __cxa_throw,
            __gxx_personality_v0,
            _divide_number,
            _print_c_string,
        }
    }
}

pub extern "C" fn println_bool(value: u8) {
    println!("{}", value == 1);
}

pub extern "C" fn println_number(value: i32) {
    println!("{}", value);
}

pub extern "C" fn _divide_number(x: i32, y: i32) -> i32 {
    if y == 0 {
        unsafe {
            let exception_object = __cxa_allocate_exception(std::mem::size_of::<*const str>());
            let exception = CString::new(format!("Attempted to divide {} by zero.", x)).unwrap();

            std::ptr::write(exception_object, exception.into_raw());

            __cxa_throw(exception_object, null(), None)
        }
    } else {
        x / y
    }
}

pub extern "C" fn _print_c_string(string: *mut c_char) {
    println!("{}", unsafe { CString::from_raw(string) }.to_str().unwrap());
}
