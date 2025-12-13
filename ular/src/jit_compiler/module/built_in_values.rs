use crate::{
    arguments::GarbageCollectionPlan,
    jit_compiler::{
        get_value_buffer_type,
        module::UlarModule,
        scope::LocalName,
        value::{UlarFunction, UlarValue},
    },
    mmtk::runtime::{mmtk_alloc, mmtk_bind_current_mutator, mmtk_bind_mutator, mmtk_init},
    parser::type_::NumericType,
    phase::built_in_values::{BuiltInPathBuf, BuiltInValueProducer, BuiltInValues},
};
use dyn_clone::DynClone;
use hashbrown::Equivalent;
use inkwell::{
    builder::Builder,
    context::Context,
    execution_engine::ExecutionEngine,
    module::Linkage,
    types::{ArrayType, FunctionType, IntType, StructType},
    values::{BasicValue, FunctionValue},
    AddressSpace, GlobalVisibility,
};
use num::Zero;
use std::{
    cell::OnceCell,
    collections::HashMap,
    ffi::{c_char, CString},
    fmt::{Display, Formatter},
    hash::Hash,
    ops::Div,
    ptr::null,
};
use ular_scheduler::{Job, ThreadSpawner, Worker, WorkerPool, VALUE_BUFFER_WORD_SIZE};

extern "C" {
    fn __cxa_allocate_exception(size: usize) -> *mut *mut c_char;
    fn __cxa_throw(
        exception_object: *mut *mut c_char,
        type_info: *const i8,
        destructor: Option<extern "C" fn(*mut *mut c_char)>,
    ) -> !;
}

pub struct BuiltInDependencies<'a, 'context> {
    context: &'context Context,
    execution_engine: &'a ExecutionEngine<'context>,
    module: &'a UlarModule<'context>,
}

pub trait BuiltInValue<'a>: DynClone {
    fn get_value(
        &self,
        dependencies: &BuiltInDependencies<'_, 'a>,
        local_name: LocalName,
        builder: &Builder<'a>,
    ) -> UlarValue<'a>;
}

dyn_clone::clone_trait_object!(BuiltInValue<'_>);

#[derive(Clone)]
pub struct BuiltInBool<'a> {
    value: u64,
    computed_value: OnceCell<UlarValue<'a>>,
}

impl BuiltInBool<'_> {
    pub fn new(value: u64) -> Self {
        Self {
            value,
            computed_value: OnceCell::new(),
        }
    }
}

impl<'a> BuiltInValue<'a> for BuiltInBool<'a> {
    fn get_value(
        &self,
        dependencies: &BuiltInDependencies<'_, 'a>,
        local_name: LocalName,
        builder: &Builder<'a>,
    ) -> UlarValue<'a> {
        let BuiltInDependencies {
            context, module, ..
        } = dependencies;

        *self.computed_value.get_or_init(|| {
            let global = module.add_global(context.i8_type());

            global.set_visibility(GlobalVisibility::Hidden);
            global.set_constant(true);
            global.set_initializer(
                &context
                    .i8_type()
                    .const_int(self.value, false)
                    .as_basic_value_enum(),
            );

            builder
                .build_load(
                    context.i8_type(),
                    global.as_pointer_value(),
                    &local_name.to_string(),
                )
                .unwrap()
                .into_int_value()
                .into()
        })
    }
}

pub trait BuiltInFunction<'a> {
    fn build_function(&self, dependencies: &BuiltInDependencies<'_, 'a>) -> FunctionValue<'a>;
    fn stored_function(&self) -> &OnceCell<FunctionValue<'a>>;

    fn get_inkwell_function(
        &self,
        built_in_values: &JitCompilerBuiltInValues<'_, 'a>,
    ) -> FunctionValue<'a> {
        self.get_inkwell_function_with_dependencies(&built_in_values.dependencies)
    }

    fn get_inkwell_function_with_dependencies(
        &self,
        dependencies: &BuiltInDependencies<'_, 'a>,
    ) -> FunctionValue<'a> {
        *self
            .stored_function()
            .get_or_init(|| self.build_function(dependencies))
    }
}

impl<'a, A: BuiltInFunction<'a> + DynClone> BuiltInValue<'a> for A {
    fn get_value(
        &self,
        dependencies: &BuiltInDependencies<'_, 'a>,
        _local_name: LocalName,
        _builder: &Builder<'a>,
    ) -> UlarValue<'a> {
        UlarValue::Function(UlarFunction::DirectReference(
            self.get_inkwell_function_with_dependencies(dependencies),
        ))
    }
}

#[derive(Clone)]
pub struct BuiltInLinkedFunction<'a> {
    name: String,
    signature: FunctionType<'a>,
    stored_function: OnceCell<FunctionValue<'a>>,
}

impl<'a> BuiltInLinkedFunction<'a> {
    pub fn new(name: String, signature: FunctionType<'a>) -> Self {
        Self {
            name,
            signature,
            stored_function: OnceCell::new(),
        }
    }
}

impl<'a> BuiltInFunction<'a> for BuiltInLinkedFunction<'a> {
    fn build_function(&self, dependencies: &BuiltInDependencies<'_, 'a>) -> FunctionValue<'a> {
        dependencies.module.underlying.add_function(
            &self.name,
            self.signature,
            Some(Linkage::External),
        )
    }

    fn stored_function(&self) -> &OnceCell<FunctionValue<'a>> {
        &self.stored_function
    }
}

#[derive(Clone)]
pub struct BuiltInMappedFunction<'a> {
    name: String,
    signature: FunctionType<'a>,
    address: usize,
    stored_function: OnceCell<FunctionValue<'a>>,
}

impl<'a> BuiltInMappedFunction<'a> {
    pub fn new(name: String, signature: FunctionType<'a>, address: usize) -> Self {
        Self {
            name,
            signature,
            address,
            stored_function: OnceCell::new(),
        }
    }
}

impl<'a> BuiltInFunction<'a> for BuiltInMappedFunction<'a> {
    fn build_function(&self, dependencies: &BuiltInDependencies<'_, 'a>) -> FunctionValue<'a> {
        let BuiltInDependencies {
            execution_engine,
            module,
            ..
        } = dependencies;

        let result = module
            .underlying
            .add_function(&self.name, self.signature, None);

        execution_engine.add_global_mapping(&result, self.address);

        result
    }

    fn stored_function(&self) -> &OnceCell<FunctionValue<'a>> {
        &self.stored_function
    }
}

struct JitCompilerBuiltInValueProvider<'a> {
    context: &'a Context,
}

impl<'a> BuiltInValueProducer for JitCompilerBuiltInValueProvider<'a> {
    type Value = Box<dyn BuiltInValue<'a> + 'a>;

    fn get_println_bool(&self, name: &str) -> Self::Value {
        Box::new(BuiltInMappedFunction::new(
            name.to_owned(),
            self.context.void_type().fn_type(
                &[
                    self.context.ptr_type(AddressSpace::default()).into(),
                    self.context.i8_type().into(),
                ],
                false,
            ),
            println_bool as usize,
        ))
    }

    fn get_println_numeric(&self, name: &str, numeric_type: NumericType) -> Self::Value {
        let address = match numeric_type {
            NumericType::I8 => println_display::<i8> as usize,
            NumericType::I16 => println_display::<i16> as usize,
            NumericType::I32 => println_display::<i32> as usize,
            NumericType::I64 => println_display::<i64> as usize,
            NumericType::U8 => println_display::<u8> as usize,
            NumericType::U16 => println_display::<u16> as usize,
            NumericType::U32 => println_display::<u32> as usize,
            NumericType::U64 => println_display::<u64> as usize,
        };

        Box::new(BuiltInMappedFunction::new(
            name.to_owned(),
            self.context.void_type().fn_type(
                &[
                    self.context.ptr_type(AddressSpace::default()).into(),
                    numeric_type.inkwell_type(self.context).into(),
                ],
                false,
            ),
            address,
        ))
    }

    fn get_println_str(&self, name: &str) -> Self::Value {
        Box::new(BuiltInMappedFunction::new(
            name.to_owned(),
            self.context.void_type().fn_type(
                &[
                    self.context.ptr_type(AddressSpace::default()).into(),
                    // https://llvm.org/docs/Statepoints.html#rewritestatepointsforgc
                    self.context.ptr_type(AddressSpace::from(1)).into(),
                ],
                false,
            ),
            println_display::<&UlarString> as usize,
        ))
    }

    fn get_true(&self, _name: &str) -> Self::Value {
        Box::new(BuiltInBool::new(1))
    }

    fn get_false(&self, _name: &str) -> Self::Value {
        Box::new(BuiltInBool::new(0))
    }
}

pub struct JitCompilerBuiltInValues<'a, 'context> {
    dependencies: BuiltInDependencies<'a, 'context>,
    underlying: BuiltInValues<JitCompilerBuiltInValueProvider<'context>>,
    pub(in crate::jit_compiler) __cxa_allocate_exception: BuiltInLinkedFunction<'context>,
    pub(in crate::jit_compiler) __cxa_begin_catch: BuiltInLinkedFunction<'context>,
    pub(in crate::jit_compiler) __cxa_end_catch: BuiltInLinkedFunction<'context>,
    pub(in crate::jit_compiler) __cxa_throw: BuiltInLinkedFunction<'context>,
    pub(in crate::jit_compiler) __gxx_personality_v0: BuiltInLinkedFunction<'context>,
    pub(in crate::jit_compiler) _divide_i8: BuiltInMappedFunction<'context>,
    pub(in crate::jit_compiler) _divide_i16: BuiltInMappedFunction<'context>,
    pub(in crate::jit_compiler) _divide_i32: BuiltInMappedFunction<'context>,
    pub(in crate::jit_compiler) _divide_i64: BuiltInMappedFunction<'context>,
    pub(in crate::jit_compiler) _divide_u8: BuiltInMappedFunction<'context>,
    pub(in crate::jit_compiler) _divide_u16: BuiltInMappedFunction<'context>,
    pub(in crate::jit_compiler) _divide_u32: BuiltInMappedFunction<'context>,
    pub(in crate::jit_compiler) _divide_u64: BuiltInMappedFunction<'context>,
    pub(in crate::jit_compiler) _garbage_collection_plan_type: IntType<'context>,
    pub(in crate::jit_compiler) _job_new: BuiltInMappedFunction<'context>,
    pub(in crate::jit_compiler) _job_type: StructType<'context>,
    pub(in crate::jit_compiler) _mmtk_alloc: BuiltInMappedFunction<'context>,
    pub(in crate::jit_compiler) _mmtk_bind_current_mutator: BuiltInMappedFunction<'context>,
    pub(in crate::jit_compiler) _mmtk_init: BuiltInMappedFunction<'context>,
    pub(in crate::jit_compiler) _print_c_string: BuiltInMappedFunction<'context>,
    pub(in crate::jit_compiler) _value_buffer_option_type: StructType<'context>,
    pub(in crate::jit_compiler) _value_buffer_type: ArrayType<'context>,
    pub(in crate::jit_compiler) _workerpool_join: BuiltInMappedFunction<'context>,
    pub(in crate::jit_compiler) _workerpool_new: BuiltInMappedFunction<'context>,
    pub(in crate::jit_compiler) _workerpool_worker: BuiltInMappedFunction<'context>,
    pub(in crate::jit_compiler) _worker_fork: BuiltInMappedFunction<'context>,
    pub(in crate::jit_compiler) _worker_free: BuiltInMappedFunction<'context>,
    pub(in crate::jit_compiler) _worker_tick: BuiltInMappedFunction<'context>,
    pub(in crate::jit_compiler) _worker_try_join: BuiltInMappedFunction<'context>,
}

impl<'a, 'context> JitCompilerBuiltInValues<'a, 'context> {
    pub(in crate::jit_compiler) fn get<Path: Equivalent<BuiltInPathBuf> + Hash>(
        &self,
        path: &Path,
        local_name: LocalName,
        builder: &Builder<'context>,
    ) -> Option<UlarValue<'context>> {
        Some(
            self.underlying
                .get(path)?
                .get_value(&self.dependencies, local_name, builder),
        )
    }

    pub(in crate::jit_compiler) fn get_division_function(
        &self,
        numeric_type: NumericType,
    ) -> &BuiltInMappedFunction<'context> {
        match numeric_type {
            NumericType::I8 => &self._divide_i8,
            NumericType::I16 => &self._divide_i16,
            NumericType::I32 => &self._divide_i32,
            NumericType::I64 => &self._divide_i64,
            NumericType::U8 => &self._divide_u8,
            NumericType::U16 => &self._divide_u16,
            NumericType::U32 => &self._divide_u32,
            NumericType::U64 => &self._divide_u64,
        }
    }

    pub(in crate::jit_compiler) fn new(
        context: &'context Context,
        execution_engine: &'a ExecutionEngine<'context>,
        module: &'a UlarModule<'context>,
        additional_values: HashMap<String, Box<dyn BuiltInValue<'context> + 'context>>,
    ) -> Self {
        let dependencies = BuiltInDependencies {
            context,
            execution_engine,
            module,
        };

        let provider = JitCompilerBuiltInValueProvider { context };
        let underlying = BuiltInValues::new(provider, additional_values);
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
            /*
             * TODO: Return the job directly once this issue is fixed:
             * https://github.com/llvm/llvm-project/issues/156254
             */
            context
                .void_type()
                .fn_type(&[context.ptr_type(AddressSpace::default()).into()], false),
            _job_new as usize,
        );

        let _mmtk_alloc = BuiltInMappedFunction::new(
            String::from("_mmtk_alloc"),
            context
                // https://llvm.org/docs/Statepoints.html#rewritestatepointsforgc
                .ptr_type(AddressSpace::from(1))
                .fn_type(&[pointer_sized_int_type, pointer_sized_int_type], false),
            mmtk_alloc as usize,
        );

        let _mmtk_bind_current_mutator = BuiltInMappedFunction::new(
            String::from("_mmtk_bind_current_mutator"),
            context.void_type().fn_type(&[], false),
            mmtk_bind_current_mutator as usize,
        );

        let _garbage_collection_plan_type =
            context.custom_width_int_type(size_of::<GarbageCollectionPlan>() as u32 * 8);

        let _mmtk_init = BuiltInMappedFunction::new(
            String::from("_mmtk_init"),
            context
                .void_type()
                .fn_type(&[_garbage_collection_plan_type.into()], false),
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
            dependencies,
            underlying,
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
            _garbage_collection_plan_type,
            _job_new,
            _job_type,
            _mmtk_alloc,
            _mmtk_bind_current_mutator,
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

#[repr(C)]
struct UlarString {
    object_descriptor_reference: u32,
    length: usize,
    content: [u8],
}

impl Display for UlarString {
    fn fmt(&self, formatter: &mut Formatter) -> std::fmt::Result {
        // SAFETY: `self.length` is the length of `self.content`
        let slice = unsafe { std::slice::from_raw_parts(self.content.as_ptr(), self.length) };

        // SAFETY: Ular strings always contain valid UTF-8
        let string = unsafe { std::str::from_utf8_unchecked(slice) };

        write!(formatter, "{}", string)
    }
}

struct UlarThreadSpawner {}

impl ThreadSpawner for UlarThreadSpawner {
    fn spawn_thread<A: FnOnce() + Send + 'static>(callback: A) -> std::thread::JoinHandle<()> {
        let handle = std::thread::spawn(callback);

        mmtk_bind_mutator(handle.thread().clone());

        handle
    }
}

extern "C" fn println_bool(_worker: &Worker, value: u8) {
    println!("{}", value == 1);
}

extern "C" fn println_display<A: Display>(_worker: &Worker, value: A) {
    println!("{}", value);
}

extern "C" fn _divide_number<A: Display + Div<Output = A> + Zero>(x: A, y: A) -> A {
    if y.is_zero() {
        throw_exception(CString::new(format!("Attempted to divide {} by zero.", x)).unwrap())
    } else {
        x / y
    }
}

/// # Safety
///
/// This function should only be called with a pointer that's safe to write to.
unsafe extern "C" fn _job_new(job: *mut Job<(), ()>) {
    *job = Job::new();
}

/// # Safety
///
/// This function should only be called with a pointer that's safe to pass to [CString::from_raw].
unsafe extern "C" fn _print_c_string(string: *mut c_char) {
    println!("{}", CString::from_raw(string).to_str().unwrap());
}

extern "C" fn _workerpool_new() -> *mut WorkerPool {
    Box::into_raw(Box::new(WorkerPool::new::<UlarThreadSpawner>(
        ular_scheduler::Configuration::default(),
    )))
}

/// # Safety
///
/// This function should only be called with a pointer that's safe to pass to [Box::from_raw].
unsafe extern "C" fn _workerpool_join(worker_pool: *mut WorkerPool) {
    if let Err(error) = Box::from_raw(worker_pool).join() {
        throw_exception(CString::new(format!("Failed to join worker pool: {:?}", error)).unwrap())
    }
}

/// # Safety
///
/// This function should only be called with a pointer that's safe to pass to [Box::from_raw].
unsafe extern "C" fn _worker_free(worker: *mut Worker) {
    drop(Box::from_raw(worker));
}

/// # Safety
///
/// This function should only be called with a pointer that's convertible to a `&WorkerPool`.
unsafe extern "C" fn _workerpool_worker(worker_pool: *const WorkerPool) -> *mut Worker {
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
