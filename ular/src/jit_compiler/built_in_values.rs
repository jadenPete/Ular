use crate::{
    jit_compiler::{
        get_value_buffer_type,
        module::UlarModule,
        scope::LocalName,
        value::{UlarFunction, UlarValue},
    },
    mmtk::built_in_functions::{mmtk_alloc, mmtk_bind_mutator, mmtk_init},
    parser::type_::NumericType,
};
use inkwell::{
    builder::Builder,
    context::Context,
    execution_engine::ExecutionEngine,
    module::Linkage,
    types::{ArrayType, FunctionType, StructType},
    values::{BasicValue, FunctionValue},
    AddressSpace, GlobalVisibility,
};
use num::Zero;
use std::{
    collections::HashMap,
    ffi::{c_char, CString},
    fmt::Display,
    ops::Div,
    ptr::null,
};
use ular_scheduler::{Job, Worker, WorkerPool, VALUE_BUFFER_WORD_SIZE};

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

pub struct BuiltInBool<'a> {
    value: u64,
    computed_value: Option<UlarValue<'a>>,
}

impl BuiltInBool<'_> {
    fn new(value: u64) -> Self {
        Self {
            value,
            computed_value: None,
        }
    }
}

impl<'a> BuiltInValue<'a> for BuiltInBool<'a> {
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
        _local_name: LocalName,
        context: &'a Context,
        _builder: &Builder<'a>,
        execution_engine: &ExecutionEngine<'a>,
        module: &mut UlarModule<'a>,
    ) -> UlarValue<'a> {
        UlarValue::Function(UlarFunction::DirectReference(self.get_inkwell_function(
            context,
            execution_engine,
            module,
        )))
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
    pub _divide_i8: BuiltInMappedFunction<'a>,
    pub _divide_i16: BuiltInMappedFunction<'a>,
    pub _divide_i32: BuiltInMappedFunction<'a>,
    pub _divide_i64: BuiltInMappedFunction<'a>,
    pub _divide_u8: BuiltInMappedFunction<'a>,
    pub _divide_u16: BuiltInMappedFunction<'a>,
    pub _divide_u32: BuiltInMappedFunction<'a>,
    pub _divide_u64: BuiltInMappedFunction<'a>,
    pub _job_new: BuiltInMappedFunction<'a>,
    pub _job_type: StructType<'a>,
    pub _mmtk_alloc: BuiltInMappedFunction<'a>,
    pub _mmtk_bind_mutator: BuiltInMappedFunction<'a>,
    pub _mmtk_init: BuiltInMappedFunction<'a>,
    pub _print_c_string: BuiltInMappedFunction<'a>,
    pub _value_buffer_option_type: StructType<'a>,
    pub _value_buffer_type: ArrayType<'a>,
    pub _workerpool_join: BuiltInMappedFunction<'a>,
    pub _workerpool_new: BuiltInMappedFunction<'a>,
    pub _workerpool_worker: BuiltInMappedFunction<'a>,
    pub _worker_fork: BuiltInMappedFunction<'a>,
    pub _worker_free: BuiltInMappedFunction<'a>,
    pub _worker_tick: BuiltInMappedFunction<'a>,
    pub _worker_try_join: BuiltInMappedFunction<'a>,
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

    pub fn get_division_function_mut(
        &mut self,
        numeric_type: NumericType,
    ) -> &mut BuiltInMappedFunction<'a> {
        match numeric_type {
            NumericType::I8 => &mut self._divide_i8,
            NumericType::I16 => &mut self._divide_i16,
            NumericType::I32 => &mut self._divide_i32,
            NumericType::I64 => &mut self._divide_i64,
            NumericType::U8 => &mut self._divide_u8,
            NumericType::U16 => &mut self._divide_u16,
            NumericType::U32 => &mut self._divide_u32,
            NumericType::U64 => &mut self._divide_u64,
        }
    }

    pub fn new(context: &'a Context, execution_engine: &ExecutionEngine<'a>) -> Self {
        let mut referencable_values = HashMap::<String, Box<dyn BuiltInValue>>::new();

        referencable_values.insert(String::from("true"), Box::new(BuiltInBool::new(1)));
        referencable_values.insert(String::from("false"), Box::new(BuiltInBool::new(0)));
        referencable_values.insert(
            String::from("println_bool"),
            Box::new(BuiltInMappedFunction::new(
                String::from("println_bool"),
                context.void_type().fn_type(
                    &[
                        context.ptr_type(AddressSpace::default()).into(),
                        context.i8_type().into(),
                    ],
                    false,
                ),
                println_bool as usize,
            )),
        );

        referencable_values.insert(
            String::from("println_i8"),
            Box::new(BuiltInMappedFunction::new(
                String::from("println_i8"),
                context.void_type().fn_type(
                    &[
                        context.ptr_type(AddressSpace::default()).into(),
                        context.i8_type().into(),
                    ],
                    false,
                ),
                println_display::<i8> as usize,
            )),
        );

        referencable_values.insert(
            String::from("println_i16"),
            Box::new(BuiltInMappedFunction::new(
                String::from("println_i16"),
                context.void_type().fn_type(
                    &[
                        context.ptr_type(AddressSpace::default()).into(),
                        context.i16_type().into(),
                    ],
                    false,
                ),
                println_display::<i16> as usize,
            )),
        );

        referencable_values.insert(
            String::from("println_i32"),
            Box::new(BuiltInMappedFunction::new(
                String::from("println_i32"),
                context.void_type().fn_type(
                    &[
                        context.ptr_type(AddressSpace::default()).into(),
                        context.i32_type().into(),
                    ],
                    false,
                ),
                println_display::<i32> as usize,
            )),
        );

        referencable_values.insert(
            String::from("println_i64"),
            Box::new(BuiltInMappedFunction::new(
                String::from("println_i64"),
                context.void_type().fn_type(
                    &[
                        context.ptr_type(AddressSpace::default()).into(),
                        context.i64_type().into(),
                    ],
                    false,
                ),
                println_display::<i64> as usize,
            )),
        );

        referencable_values.insert(
            String::from("println_u8"),
            Box::new(BuiltInMappedFunction::new(
                String::from("println_u8"),
                context.void_type().fn_type(
                    &[
                        context.ptr_type(AddressSpace::default()).into(),
                        context.i8_type().into(),
                    ],
                    false,
                ),
                println_display::<u8> as usize,
            )),
        );

        referencable_values.insert(
            String::from("println_u16"),
            Box::new(BuiltInMappedFunction::new(
                String::from("println_u16"),
                context.void_type().fn_type(
                    &[
                        context.ptr_type(AddressSpace::default()).into(),
                        context.i16_type().into(),
                    ],
                    false,
                ),
                println_display::<u16> as usize,
            )),
        );

        referencable_values.insert(
            String::from("println_u32"),
            Box::new(BuiltInMappedFunction::new(
                String::from("println_u32"),
                context.void_type().fn_type(
                    &[
                        context.ptr_type(AddressSpace::default()).into(),
                        context.i32_type().into(),
                    ],
                    false,
                ),
                println_display::<u32> as usize,
            )),
        );

        referencable_values.insert(
            String::from("println_u64"),
            Box::new(BuiltInMappedFunction::new(
                String::from("println_u64"),
                context.void_type().fn_type(
                    &[
                        context.ptr_type(AddressSpace::default()).into(),
                        context.i64_type().into(),
                    ],
                    false,
                ),
                println_display::<u64> as usize,
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

        let pointer_sized_int_type = context
            .ptr_sized_int_type(execution_engine.get_target_data(), None)
            .into();

        let _divide_i8 = divide_built_in_function::<i8>(context, NumericType::I8);
        let _divide_i16 = divide_built_in_function::<i16>(context, NumericType::I16);
        let _divide_i32 = divide_built_in_function::<i32>(context, NumericType::I32);
        let _divide_i64 = divide_built_in_function::<i64>(context, NumericType::I64);
        let _divide_u8 = divide_built_in_function::<u8>(context, NumericType::U8);
        let _divide_u16 = divide_built_in_function::<u16>(context, NumericType::U16);
        let _divide_u32 = divide_built_in_function::<u32>(context, NumericType::U32);
        let _divide_u64 = divide_built_in_function::<u64>(context, NumericType::U64);
        let job_link_type = context.opaque_struct_type("job_link_type");

        job_link_type.set_body(
            &[
                context.ptr_type(AddressSpace::default()).into(), // `prev`
                context.ptr_type(AddressSpace::default()).into(), // `next`
            ],
            false,
        );

        let _job_type = context.opaque_struct_type("job_type");

        _job_type.set_body(
            &[
                context.i8_type().into(),                         // `done`
                job_link_type.into(),                             // `link`
                context.ptr_type(AddressSpace::default()).into(), // `function`
                context.ptr_type(AddressSpace::default()).into(), // `context`
                get_value_buffer_type(context).into(),            // `result`
            ],
            false,
        );

        let _job_new = BuiltInMappedFunction::new(
            String::from("_job_new"),
            _job_type.fn_type(&[], false),
            Job::<(), ()>::new as usize,
        );

        let _mmtk_alloc = BuiltInMappedFunction::new(
            String::from("_mmtk_alloc"),
            context
                .ptr_type(AddressSpace::default())
                .fn_type(&[pointer_sized_int_type, pointer_sized_int_type], false),
            mmtk_alloc as usize,
        );

        let _mmtk_bind_mutator = BuiltInMappedFunction::new(
            String::from("_mmtk_bind_mutator"),
            context
                .void_type()
                .fn_type(&[context.ptr_type(AddressSpace::default()).into()], false),
            mmtk_bind_mutator as usize,
        );

        let _mmtk_init = BuiltInMappedFunction::new(
            String::from("_mmtk_init"),
            context.void_type().fn_type(&[], false),
            mmtk_init as usize,
        );

        let _print_c_string = BuiltInMappedFunction::new(
            String::from("_print_c_string"),
            context
                .void_type()
                .fn_type(&[context.ptr_type(AddressSpace::default()).into()], false),
            _print_c_string as usize,
        );

        let _workerpool_new = BuiltInMappedFunction::new(
            String::from("_workerpool_new"),
            context
                .ptr_type(AddressSpace::default())
                .fn_type(&[], false),
            _workerpool_new as usize,
        );

        let _workerpool_join = BuiltInMappedFunction::new(
            String::from("_workerpool_join"),
            context
                .void_type()
                .fn_type(&[context.ptr_type(AddressSpace::default()).into()], false),
            _workerpool_join as usize,
        );

        let _workerpool_worker = BuiltInMappedFunction::new(
            String::from("_workerpool_worker"),
            context
                .ptr_type(AddressSpace::default())
                .fn_type(&[context.ptr_type(AddressSpace::default()).into()], false),
            _workerpool_worker as usize,
        );

        let _worker_fork = BuiltInMappedFunction::new(
            String::from("_worker_fork"),
            context.void_type().fn_type(
                &[
                    context.ptr_type(AddressSpace::default()).into(),
                    context.ptr_type(AddressSpace::default()).into(),
                    context.ptr_type(AddressSpace::default()).into(),
                    context.ptr_type(AddressSpace::default()).into(),
                ],
                false,
            ),
            Worker::fork::<(), ()> as usize,
        );

        let _worker_free = BuiltInMappedFunction::new(
            String::from("_worker_free"),
            context
                .void_type()
                .fn_type(&[context.ptr_type(AddressSpace::default()).into()], false),
            _worker_free as usize,
        );

        let _worker_tick = BuiltInMappedFunction::new(
            String::from("_worker_tick"),
            context
                .void_type()
                .fn_type(&[context.ptr_type(AddressSpace::default()).into()], false),
            Worker::tick as usize,
        );

        let _value_buffer_type = context.i64_type().array_type(VALUE_BUFFER_WORD_SIZE as u32);
        let _value_buffer_option_type = context.opaque_struct_type("option_value_buffer_type");

        _value_buffer_option_type.set_body(
            &[
                context.i8_type().into(),  // The discriminator
                _value_buffer_type.into(), // The value buffer
            ],
            false,
        );

        let _worker_try_join = BuiltInMappedFunction::new(
            String::from("_worker_try_join"),
            _value_buffer_option_type.fn_type(
                &[
                    context.ptr_type(AddressSpace::default()).into(),
                    context.ptr_type(AddressSpace::default()).into(),
                ],
                false,
            ),
            Worker::try_join::<(), ()> as usize,
        );

        Self {
            referencable_values,
            __cxa_allocate_exception,
            __cxa_begin_catch,
            __cxa_end_catch,
            __cxa_throw,
            __gxx_personality_v0,
            _divide_i8,
            _divide_i16,
            _divide_i32,
            _divide_i64,
            _divide_u8,
            _divide_u16,
            _divide_u32,
            _divide_u64,
            _job_new,
            _job_type,
            _mmtk_alloc,
            _mmtk_bind_mutator,
            _mmtk_init,
            _print_c_string,
            _value_buffer_option_type,
            _value_buffer_type,
            _workerpool_new,
            _workerpool_join,
            _workerpool_worker,
            _worker_fork,
            _worker_free,
            _worker_tick,
            _worker_try_join,
        }
    }
}

pub extern "C" fn println_bool(_worker: &Worker, value: u8) {
    println!("{}", value == 1);
}

pub extern "C" fn println_display<A: Display>(_worker: &Worker, value: A) {
    println!("{}", value);
}

pub extern "C" fn _divide_number<A: Display + Div<Output = A> + Zero>(x: A, y: A) -> A {
    if y.is_zero() {
        throw_exception(CString::new(format!("Attempted to divide {} by zero.", x)).unwrap())
    } else {
        x / y
    }
}

/// # Safety
///
/// This function should only be called with a pointer that's safe to pass to [CString::from_raw].
pub unsafe extern "C" fn _print_c_string(string: *mut c_char) {
    println!("{}", CString::from_raw(string).to_str().unwrap());
}

pub extern "C" fn _workerpool_new() -> *mut WorkerPool {
    Box::into_raw(Box::new(WorkerPool::new(ular_scheduler::Configuration {
        background_thread_initializer: Some(Box::new(mmtk_bind_mutator)),
        ..ular_scheduler::Configuration::default()
    })))
}

/// # Safety
///
/// This function should only be called with a pointer that's safe to pass to [Box::from_raw].
pub unsafe extern "C" fn _workerpool_join(worker_pool: *mut WorkerPool) {
    if let Err(error) = Box::from_raw(worker_pool).join() {
        throw_exception(CString::new(format!("Failed to join worker pool: {:?}", error)).unwrap())
    }
}

/// # Safety
///
/// This function should only be called with a pointer that's safe to pass to [Box::from_raw].
pub unsafe extern "C" fn _worker_free(worker: *mut Worker) {
    drop(Box::from_raw(worker));
}

/// # Safety
///
/// This function should only be called with a pointer that's confertable to a `&WorkerPool`.
pub unsafe extern "C" fn _workerpool_worker(worker_pool: *const WorkerPool) -> *mut Worker {
    Box::into_raw(Box::new(worker_pool.as_ref().unwrap().worker()))
}

fn divide_built_in_function<A: Display + Div<Output = A> + Zero>(
    context: &Context,
    numeric_type: NumericType,
) -> BuiltInMappedFunction<'_> {
    BuiltInMappedFunction::new(
        format!("_divide_{}", numeric_type),
        numeric_type.inkwell_type(context).fn_type(
            &[
                numeric_type.inkwell_type(context).into(),
                numeric_type.inkwell_type(context).into(),
            ],
            false,
        ),
        _divide_number::<A> as usize,
    )
}

/// It's important that this function is inlined. If it's not, [__cxa_throw] may have trouble
/// unwinding the call stack because [throw_exception] isn't an `extern "C"` function.
#[inline(always)]
fn throw_exception(message: CString) -> ! {
    unsafe {
        let exception_object = __cxa_allocate_exception(std::mem::size_of::<*const str>());

        std::ptr::write(exception_object, message.into_raw());

        __cxa_throw(exception_object, null(), None)
    }
}
