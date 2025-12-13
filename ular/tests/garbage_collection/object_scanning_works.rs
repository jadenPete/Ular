use inkwell::{context::Context, AddressSpace};
use mmtk::{
    util::{Address, ObjectReference},
    vm::{slot::SimpleSlot, SlotVisitor},
};
use std::{collections::HashMap, sync::Once};
use ular::{
    arguments::Arguments,
    jit_compiler::module::built_in_values::BuiltInMappedFunction,
    mmtk::runtime::mmtk_scan_object,
    parser::type_::{FunctionType, Type},
    run_phases, AdditionalValue,
};
use ular_scheduler::Worker;

static TEST_ROOT_SCANNING_CALLED: Once = Once::new();

struct MockSlotVisitor {
    visited_addresses: Vec<Address>,
}

impl MockSlotVisitor {
    fn new() -> Self {
        Self {
            visited_addresses: Vec::new(),
        }
    }
}

impl SlotVisitor<SimpleSlot> for MockSlotVisitor {
    fn visit_slot(&mut self, slot: SimpleSlot) {
        self.visited_addresses.push(slot.as_address())
    }
}

extern "C" fn __test_root_scanning(_worker: *mut Worker, student: ObjectReference) {
    let mut slot_visitor = MockSlotVisitor::new();

    mmtk_scan_object(student, &mut slot_visitor);

    let visited_offsets = slot_visitor
        .visited_addresses
        .iter()
        .map(|address| address.as_usize() - student.to_raw_address().as_usize())
        .collect::<Vec<_>>();

    // We're testing that when we call `mmtk_scan_object` with a `Student` object, the offset to the
    // `teacher` field is returned. The offset should be 8 bytes (64 bits)—32 bits for the
    // object descriptor entry, 8 bits for the `age` field, and 24 bits of padding so the `Teacher`
    // reference is a multiple of 8.
    assert_eq!(visited_offsets, [8]);

    TEST_ROOT_SCANNING_CALLED.call_once(|| {});
}

#[test]
fn test() -> anyhow::Result<()> {
    let context = Context::create();
    let source = "\
struct Teacher {
    age: u8;
}
            
struct Student {
    age: u8;
    teacher: Teacher;
    grade: u8;
}

__test_root_scanning(Student {
    age: 11,
    teacher: Teacher {
        age: 30
    },
    
    grade: 6
});
";

    let mut additional_values = HashMap::new();

    additional_values.insert(
        String::from("__test_root_scanning"),
        AdditionalValue {
            built_in_value: Box::new(BuiltInMappedFunction::new(
                String::from("__test_root_scanning"),
                context
                    .void_type()
                    .fn_type(&[context.ptr_type(AddressSpace::default()).into()], false),
                __test_root_scanning as usize,
            )),

            type_: Type::Function(FunctionType {
                parameters: vec![Type::Identifier(String::from("Student"))],
                return_type: Box::new(Type::Unit),
            }),
        },
    );

    run_phases(&context, source, &Arguments::default(), additional_values)?;

    assert!(TEST_ROOT_SCANNING_CALLED.is_completed());

    Ok(())
}
