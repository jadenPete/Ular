use crate::{
    error_reporting::CompilationError,
    jit_compiler::{
        get_value_buffer_type,
        module::UlarModule,
        scope::JitCompilerScope,
        value::{UlarFunction, UlarValue},
    },
};
use either::Left;
use inkwell::{
    builder::Builder,
    context::Context,
    types::{BasicTypeEnum, FunctionType, StructType},
    values::{BasicMetadataValueEnum, BasicValueEnum, FunctionValue, PointerValue},
    AddressSpace,
};
use std::{
    collections::HashMap,
    hash::{Hash, Hasher},
};

pub struct ForkFunction<'a> {
    pub function: FunctionValue<'a>,
    context_type: StructType<'a>,
    underlying_pointer: Option<PointerValue<'a>>,
}

impl<'a> ForkFunction<'a> {
    pub fn build_context(
        self,
        builder: &Builder<'a>,
        scope: &mut JitCompilerScope<'_, 'a>,
        arguments: &[UlarValue<'a>],
    ) -> Result<PointerValue<'a>, CompilationError> {
        let context_name = scope.get_local_name();
        let context = builder
            .build_alloca(self.context_type, &context_name.to_string())
            .unwrap();

        if let Some(underlying_pointer) = self.underlying_pointer {
            let context_field = builder
                .build_struct_gep(self.context_type, context, 0, &context_name.to_string())
                .unwrap();

            builder
                .build_store(context_field, underlying_pointer)
                .unwrap();
        }

        for (i, argument) in arguments.iter().enumerate() {
            let context_field_index = match self.underlying_pointer {
                Some(_) => i as u32 + 1,
                None => i as u32,
            };

            let context_field_name = scope.get_local_name();
            let context_field = builder
                .build_struct_gep(
                    self.context_type,
                    context,
                    context_field_index,
                    &context_field_name.to_string(),
                )
                .unwrap();

            builder
                .build_store(context_field, BasicValueEnum::try_from(*argument)?)
                .unwrap();
        }

        Ok(context)
    }
}

pub struct ForkFunctionCache<'a> {
    function_cache: HashMap<ForkFunctionKey<'a>, ForkFunctionValue<'a>>,
}

impl<'a> ForkFunctionCache<'a> {
    fn get_or_insert_value(
        &mut self,
        key: ForkFunctionKey<'a>,
        context: &'a Context,
        module: &UlarModule<'a>,
    ) -> ForkFunctionValue<'a> {
        let cache_length = self.function_cache.len();

        *self.function_cache.entry(key).or_insert_with(|| {
            // Skip the first parameter, which is the worker pointer
            let parameter_types = &key.get_function_type().get_param_types()[1..];
            let mut context_field_types = Vec::new();

            if let ForkFunctionKey::Indirect { .. } = key {
                context_field_types.push(context.ptr_type(AddressSpace::default()).into());
            }

            for &parameter_type in parameter_types {
                context_field_types.push(parameter_type.try_into().unwrap());
            }

            let context_type = context.struct_type(&context_field_types, false);
            let value_buffer_type = get_value_buffer_type(context);
            let function = module.underlying.add_function(
                &format!("fork_{}", cache_length),
                value_buffer_type.fn_type(
                    &[
                        context.ptr_type(AddressSpace::default()).into(),
                        context.ptr_type(AddressSpace::default()).into(),
                    ],
                    false,
                ),
                None,
            );

            let builder = context.create_builder();
            let entry_block = context.append_basic_block(function, "entry");

            builder.position_at_end(entry_block);

            let parameters = function.get_params();
            let worker_parameter = parameters[0];
            let context_parameter = parameters[1].into_pointer_value();
            let mut arguments: Vec<BasicMetadataValueEnum> = vec![worker_parameter.into()];
            let underlying_function = match key {
                ForkFunctionKey::Direct { function } => UlarFunction::DirectReference(function),
                ForkFunctionKey::Indirect { type_ } => {
                    let pointer_pointer = builder
                        .build_struct_gep(context_type, context_parameter, 0, "underlying_pointer")
                        .unwrap();

                    let pointer = builder
                        .build_load(
                            context.ptr_type(AddressSpace::default()),
                            pointer_pointer,
                            "underlying_value",
                        )
                        .unwrap()
                        .into_pointer_value();

                    UlarFunction::IndirectReference { pointer, type_ }
                }
            };

            for (i, parameter_type) in parameter_types.iter().enumerate() {
                let pointer = builder
                    .build_struct_gep(
                        context_type,
                        context_parameter,
                        match key {
                            ForkFunctionKey::Direct { .. } => i as u32,
                            ForkFunctionKey::Indirect { .. } => i as u32 + 1,
                        },
                        &format!("{}_pointer", i),
                    )
                    .unwrap();

                let value = builder
                    .build_load(
                        BasicTypeEnum::try_from(*parameter_type).unwrap(),
                        pointer,
                        &format!("{}_value", i),
                    )
                    .unwrap()
                    .into();

                arguments.push(value);
            }

            let underlying_result = match underlying_function {
                UlarFunction::DirectReference(underlying_value) => {
                    builder.build_call(underlying_value, &arguments, "result")
                }

                UlarFunction::IndirectReference { pointer, type_ } => {
                    builder.build_indirect_call(type_, pointer, &arguments, "result")
                }
            }
            .unwrap()
            .try_as_basic_value();

            let result_buffer_pointer = builder
                .build_alloca(value_buffer_type, "result_buffer_pointer")
                .unwrap();

            if let Left(underlying_result) = underlying_result {
                builder
                    .build_store(result_buffer_pointer, underlying_result)
                    .unwrap();
            }

            let result_buffer = builder
                .build_load(value_buffer_type, result_buffer_pointer, "result_buffer")
                .unwrap();

            builder.build_return(Some(&result_buffer)).unwrap();

            ForkFunctionValue {
                function,
                context_type,
            }
        })
    }

    pub fn get_or_build(
        &mut self,
        function: UlarFunction<'a>,
        context: &'a Context,
        module: &UlarModule<'a>,
    ) -> ForkFunction<'a> {
        let value =
            self.get_or_insert_value(ForkFunctionKey::for_function(function), context, module);

        ForkFunction {
            function: value.function,
            context_type: value.context_type,
            underlying_pointer: match function {
                UlarFunction::DirectReference(_) => None,
                UlarFunction::IndirectReference { pointer, .. } => Some(pointer),
            },
        }
    }

    pub fn new() -> Self {
        Self {
            function_cache: HashMap::new(),
        }
    }
}

#[derive(Clone, Copy, Eq, PartialEq)]
enum ForkFunctionKey<'a> {
    Direct { function: FunctionValue<'a> },
    Indirect { type_: FunctionType<'a> },
}

impl<'a> ForkFunctionKey<'a> {
    fn for_function(function: UlarFunction<'a>) -> Self {
        match function {
            UlarFunction::DirectReference(function) => ForkFunctionKey::Direct { function },
            UlarFunction::IndirectReference { type_, .. } => ForkFunctionKey::Indirect { type_ },
        }
    }

    fn get_function_type(&self) -> FunctionType<'a> {
        match self {
            Self::Direct { function } => function.get_type(),
            Self::Indirect { type_ } => *type_,
        }
    }
}

impl Hash for ForkFunctionKey<'_> {
    fn hash<A: Hasher>(&self, state: &mut A) {
        match self {
            ForkFunctionKey::Direct { function } => {
                state.write_u8(0);

                function.get_name().hash(state);
            }

            ForkFunctionKey::Indirect { type_ } => {
                state.write_u8(1);

                HashableFunctionType(*type_).hash(state);
            }
        }
    }
}

#[derive(Clone, Copy)]
struct ForkFunctionValue<'a> {
    function: FunctionValue<'a>,
    context_type: StructType<'a>,
}

#[derive(Eq, PartialEq)]
struct HashableFunctionType<'a>(FunctionType<'a>);

/// Although [FunctionType] implements [Eq], it doesn't implement [Hash]. Because it's known to
/// contain a pointer, we can implement a [Hash] implementation for it by reading the raw bytes behind
/// it and hashing that.
impl Hash for HashableFunctionType<'_> {
    fn hash<A: Hasher>(&self, state: &mut A) {
        let function_type_pointer: *const FunctionType = &self.0;

        // SAFETY: We're just reading the raw bytes behind `self.0`. This is safe.
        state.write(unsafe {
            std::slice::from_raw_parts(
                function_type_pointer as *const u8,
                size_of::<FunctionType>(),
            )
        })
    }
}
