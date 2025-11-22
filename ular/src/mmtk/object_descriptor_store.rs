use crate::{
    data_structures::number_map::NumberMap,
    dependency_analyzer::analyzed_program::{AnalyzedProgram, AnalyzedType},
    error_reporting::{CompilationError, CompilationErrorMessage},
};
use inkwell::{targets::TargetData, types::StructType};
use std::fmt::{Debug, Formatter};

#[derive(Debug)]
pub struct ObjectDescriptor {
    pub inner_references: Vec<ObjectInnerReference>,
    pub size: usize,
    pub align: usize,
}

#[derive(Clone, Copy)]
pub struct ObjectDescriptorReference(pub u32);

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
        let descriptor = ObjectDescriptor {
            inner_references,
            size,
            align,
        };

        self.descriptors.push(descriptor);
        self.references_by_struct_index
            .insert(struct_index, reference);

        Ok(reference)
    }

    pub fn new() -> Self {
        Self::default()
    }
}

impl Default for ObjectDescriptorStore {
    fn default() -> Self {
        Self {
            descriptors: Vec::new(),
            references_by_struct_index: NumberMap::new(0),
        }
    }
}

#[derive(Debug)]
pub struct ObjectInnerReference {
    pub offset: usize,
    pub descriptor: ObjectDescriptorReference,
}
