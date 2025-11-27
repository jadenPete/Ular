use crate::{
    data_structures::number_map::NumberMap,
    dependency_analyzer::analyzed_program::{AnalyzedProgram, AnalyzedType},
    error_reporting::{CompilationError, CompilationErrorMessage},
};
use inkwell::{context::Context, targets::TargetData, types::StructType};
use mmtk::util::ObjectReference;
use std::fmt::{Debug, Formatter};

/// The object descriptor reference for string literals.
///
/// See [crate::mmtk::UlarObjectModel] to understand why we create object descriptors for strings.
pub const STRING_LITERAL_DESCRIPTOR_INDEX: ObjectDescriptorReference = ObjectDescriptorReference(0);

/// The object descriptor reference for allocated strings.
///
/// See [crate::mmtk::UlarObjectModel] to understand why we create object descriptors for strings.
pub const STRING_ALLOCATED_DESCRIPTOR_INDEX: ObjectDescriptorReference =
    ObjectDescriptorReference(1);

#[derive(Clone, Debug)]
pub enum ObjectDescriptor {
    Struct {
        inner_references: Vec<ObjectInnerReference>,
        size: usize,
        align: usize,
    },

    String {
        length_field_offset: usize,
        length_size_difference: usize,
        align: usize,
    },
}

impl ObjectDescriptor {
    pub fn align(&self) -> usize {
        match self {
            Self::Struct { align, .. } => *align,
            Self::String { align, .. } => *align,
        }
    }

    /// Returns the size of an object, given its descriptor and a reference to it.
    ///
    /// # Safety
    ///
    /// If the object's size isn't fixed, the length of its payload must actually be located at the
    /// offset referenced in `length_field_offset`.
    pub unsafe fn get_size(&self, object: ObjectReference) -> usize {
        match self {
            Self::Struct { size, .. } => *size,
            Self::String {
                length_field_offset,
                length_size_difference,
                ..
            } => {
                let length = (object.to_raw_address() + *length_field_offset).load::<usize>();

                length + length_size_difference
            }
        }
    }

    pub fn inner_references(&self) -> &[ObjectInnerReference] {
        match self {
            Self::Struct {
                inner_references, ..
            } => inner_references,

            Self::String { .. } => &[],
        }
    }
}

#[derive(Clone, Copy, Eq, PartialEq)]
pub struct ObjectDescriptorReference(pub u32);

impl ObjectDescriptorReference {
    pub fn is_allocated(self) -> bool {
        // All structs are currently allocated on the heap, and so are strings produced at runtime.
        // Therefore, the only non-allocated objects are string literals.
        self != STRING_LITERAL_DESCRIPTOR_INDEX
    }
}

impl Debug for ObjectDescriptorReference {
    fn fmt(&self, formatter: &mut Formatter) -> std::fmt::Result {
        write!(formatter, "{}", self.0)
    }
}

#[derive(Debug)]
pub struct ObjectDescriptorStore {
    descriptors: Vec<ObjectDescriptor>,
    references_by_struct_index: NumberMap<ObjectDescriptorReference>,
}

impl ObjectDescriptorStore {
    pub fn get_descriptor(&self, reference: ObjectDescriptorReference) -> &ObjectDescriptor {
        &self.descriptors[reference.0 as usize]
    }

    pub fn get_or_set_descriptor_for_struct(
        &mut self,
        program: &AnalyzedProgram,
        struct_types: &[StructType],
        target_data: &TargetData,
        struct_index: usize,
    ) -> Result<ObjectDescriptorReference, CompilationError> {
        if let Some(reference) = self.references_by_struct_index.get(struct_index) {
            return Ok(*reference);
        }

        let reference =
            ObjectDescriptorReference(u32::try_from(self.descriptors.len()).map_err(|_| {
                CompilationError {
                    message: CompilationErrorMessage::ObjectDescriptorStoreFull,
                    position: None,
                }
            })?);

        let struct_type = &struct_types[struct_index];
        let definition = &program.structs[struct_index];
        let inner_references = definition
            .fields
            .iter()
            .enumerate()
            .flat_map(|(i, field)| match field.type_ {
                AnalyzedType::Struct(j) => Some((i, j)),
                _ => None,
            })
            .map(|(field_index, field_struct_index)| {
                let offset = target_data
                    // Add 1 to offset for the object descriptor reference
                    .offset_of_element(struct_type, field_index as u32 + 1)
                    .unwrap() as usize;

                let field_descriptor = if field_struct_index == struct_index {
                    reference
                } else {
                    self.get_or_set_descriptor_for_struct(
                        program,
                        struct_types,
                        target_data,
                        field_struct_index,
                    )?
                };

                Ok(ObjectInnerReference {
                    offset,
                    descriptor: field_descriptor,
                })
            })
            .collect::<Result<Vec<_>, _>>()?;

        let size = target_data.get_abi_size(struct_type) as usize;
        let align = target_data.get_abi_alignment(struct_type) as usize;
        let descriptor = ObjectDescriptor::Struct {
            inner_references,
            size,
            align,
        };

        self.descriptors.push(descriptor);
        self.references_by_struct_index
            .insert(struct_index, reference);

        Ok(reference)
    }

    pub fn new(context: &Context, target_data: &TargetData) -> Self {
        // In order to figure out:
        // - The offset of the string length field
        // - The alignment of string structs
        //
        // we need to know the ABI we're compiling to. I personally don't feel like rewriting all of
        // LLVM's logic for determining struct offsets and alignments, so I'd rather just create a
        // dummy string struct whose offsets and alignments we know will be identical to those of
        // any string struct, and then query the offset and alignment on that.
        let dummy_string_type = context.struct_type(
            &[
                context.i32_type().into(),
                context.ptr_sized_int_type(target_data, None).into(),
                context.i8_type().array_type(0).into(),
            ],
            false,
        );

        let string_descriptor = ObjectDescriptor::String {
            length_field_offset: target_data
                .offset_of_element(&dummy_string_type, 1)
                .unwrap() as usize,

            length_size_difference: target_data
                .offset_of_element(&dummy_string_type, 2)
                .unwrap() as usize,

            align: target_data.get_abi_alignment(&dummy_string_type) as usize,
        };

        Self {
            // One for `STRING_LITERAL_DESCRIPTOR_INDEX` and another for
            // `STRING_ALLOCATED_DESCRIPTOR_INDEX`
            descriptors: vec![string_descriptor.clone(), string_descriptor],
            references_by_struct_index: NumberMap::new(0),
        }
    }
}

#[derive(Clone, Debug)]
pub struct ObjectInnerReference {
    pub offset: usize,
    pub descriptor: ObjectDescriptorReference,
}

#[derive(Clone, Debug)]
pub enum ObjectSize {
    Fixed(usize),
    StoredAtOffset(usize),
}
