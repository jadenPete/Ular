use num::traits::FromBytes;
use std::collections::HashMap;

pub trait Parseable: Sized {
    fn parse(section: &[u8]) -> (Self, &[u8]);
}

impl<const SIZE: usize, Integer: FromBytes<Bytes = [u8; SIZE]>> Parseable for Integer {
    fn parse(section: &[u8]) -> (Self, &[u8]) {
        let mut buffer = [0; SIZE];

        buffer.copy_from_slice(&section[..SIZE]);

        (Integer::from_ne_bytes(&buffer), &section[SIZE..])
    }
}

pub struct IndexableStackMap {
    pub constants: Vec<StackMapConstant>,
    pub records_by_address: HashMap<u64, StackMapRecord>,
}

impl IndexableStackMap {
    pub fn from_stack_map(stack_map: &StackMap) -> Self {
        let mut result = Self {
            constants: stack_map.constants.clone(),
            records_by_address: HashMap::new(),
        };

        let mut i = 0;

        for function in &stack_map.functions {
            for record in &stack_map.records[i..i + function.record_count as usize] {
                result.records_by_address.insert(
                    function.address + u64::from(record.instruction_offset),
                    record.clone(),
                );
            }

            i += function.record_count as usize;
        }

        result
    }
}

pub struct StackMap {
    pub functions: Vec<StackMapFunction>,
    pub constants: Vec<StackMapConstant>,
    pub records: Vec<StackMapRecord>,
}

impl Parseable for StackMap {
    fn parse(section: &[u8]) -> (Self, &[u8]) {
        let (version, mut section) = u8::parse(section);

        assert_eq!(version, 3);

        // Skip the rest of the header
        section = &section[3..];

        let (function_count, section) = u32::parse(section);
        let (constant_count, section) = u32::parse(section);
        let (record_count, section) = u32::parse(section);
        let (functions, section) = parse_vec(function_count as usize, section);
        let (constants, section) = parse_vec(constant_count as usize, section);
        let (records, section) = parse_vec(record_count as usize, section);

        (
            Self {
                functions,
                constants,
                records,
            },
            section,
        )
    }
}

#[derive(Clone, Copy)]
pub struct StackMapConstant(pub u64);

impl Parseable for StackMapConstant {
    fn parse(section: &[u8]) -> (Self, &[u8]) {
        let (value, section) = u64::parse(section);

        (Self(value), section)
    }
}

pub struct StackMapFunction {
    pub address: u64,
    pub record_count: u64,
}

impl Parseable for StackMapFunction {
    fn parse(section: &[u8]) -> (Self, &[u8]) {
        let (address, mut section) = u64::parse(section);

        // Skip the stack size
        section = &section[8..];

        let (record_count, section) = u64::parse(section);

        (
            StackMapFunction {
                address,
                record_count,
            },
            section,
        )
    }
}

#[derive(Clone)]
pub enum StackMapLocation {
    Register(u16),
    Direct { register: u16, offset: i32 },
    Indirect { register: u16, offset: i32 },
    Constant(i32),
    ConstIndex(i32),
}

impl Parseable for StackMapLocation {
    fn parse(section: &[u8]) -> (Self, &[u8]) {
        let (encoding, mut section) = u8::parse(section);

        // Skip the reserved portion and location size
        section = &section[3..];

        let (register, mut section) = u16::parse(section);

        // Skip the second reserved portion
        section = &section[2..];

        let (offset, section) = i32::parse(section);
        let location = match encoding {
            1 => StackMapLocation::Register(register),
            2 => StackMapLocation::Direct { register, offset },
            3 => StackMapLocation::Indirect { register, offset },
            4 => StackMapLocation::Constant(offset),
            5 => StackMapLocation::ConstIndex(offset),
            _ => panic!("Unexpected stack map location encoding: {}", encoding),
        };

        (location, section)
    }
}

#[derive(Clone)]
pub struct StackMapRecord {
    pub instruction_offset: u32,
    pub locations: Vec<StackMapLocation>,
}

impl Parseable for StackMapRecord {
    fn parse(mut section: &[u8]) -> (Self, &[u8]) {
        // Skip the patch point ID
        section = &section[8..];

        let (instruction_offset, mut section) = u32::parse(section);

        // Skip the reserved portion
        section = &section[2..];

        let (location_count, section) = u16::parse(section);
        let (locations, mut section) = parse_vec(location_count as usize, section);

        // Skip the padding
        section = &section[6..];

        let (live_out_count, mut section) = u16::parse(section);

        // Skip the live outs and remaining padding
        section = &section[live_out_count as usize * 4 + 4..];

        (
            Self {
                instruction_offset,
                locations,
            },
            section,
        )
    }
}

fn parse_vec<A: Parseable>(size: usize, mut section: &[u8]) -> (Vec<A>, &[u8]) {
    let mut result = Vec::with_capacity(size);

    for _ in 0..size {
        let (element, new_section) = A::parse(section);

        section = new_section;

        result.push(element);
    }

    (result, section)
}
