use inkwell::{context::Context, AddressSpace};
use mmtk::{
    util::{Address, ObjectReference},
    vm::{slot::SimpleSlot, RootsWorkFactory},
};
use std::{
    collections::{HashMap, HashSet},
    sync::{Arc, Condvar, LazyLock, Mutex},
};
use ular::{
    arguments::Arguments,
    jit_compiler::module::built_in_values::BuiltInMappedFunction,
    mmtk::runtime::{
        mmtk_pause_all_mutators, mmtk_pause_at_safepoint, mmtk_register_roots,
        mmtk_resume_all_mutators,
    },
    parser::type_::{FunctionType, Type},
    run_phases, AdditionalValue,
};
use ular_scheduler::Worker;

static PAUSING_FOR_GARBAGE_COLLECTION: LazyLock<(Mutex<bool>, Condvar)> =
    LazyLock::new(|| (Mutex::new(false), Condvar::new()));

#[derive(Clone)]
struct MockRootsWorkFactory {
    roots: Arc<Mutex<Vec<Address>>>,
}

impl MockRootsWorkFactory {
    fn roots(&self) -> Arc<Mutex<Vec<Address>>> {
        Arc::clone(&self.roots)
    }

    fn new() -> Self {
        Self {
            roots: Arc::new(Mutex::new(Vec::new())),
        }
    }
}

impl RootsWorkFactory<SimpleSlot> for MockRootsWorkFactory {
    fn create_process_pinning_roots_work(&mut self, nodes: Vec<ObjectReference>) {
        self.roots
            .lock()
            .unwrap()
            .extend(nodes.iter().map(|node| node.to_raw_address()));
    }

    fn create_process_roots_work(&mut self, slots: Vec<SimpleSlot>) {
        self.roots
            .lock()
            .unwrap()
            .extend(slots.iter().map(|slot| slot.as_address()));
    }

    fn create_process_tpinning_roots_work(&mut self, nodes: Vec<ObjectReference>) {
        self.roots
            .lock()
            .unwrap()
            .extend(nodes.iter().map(|node| node.to_raw_address()));
    }
}

extern "C" fn __pause_for_garbage_collection(_worker: &Worker) {
    let (mutex, conditional_variable) = &*PAUSING_FOR_GARBAGE_COLLECTION;

    *mutex.lock().unwrap() = true;
    conditional_variable.notify_one();

    mmtk_pause_at_safepoint();
}

#[test]
fn test() -> anyhow::Result<()> {
    let handle = std::thread::spawn(|| {
        let context = Context::create();

        // We can't do `println_u8(person.age)` because then `person.age` may be computed before
        // `__pause_for_garbage_collection` is called, in which case `person` wouldn't be used anymore
        // and wouldn't be a root
        let source = "\
struct Person {
    age: u8;
};

fn print_person_age(person: Person) {
    seq {
        __pause_for_garbage_collection();

        age = person.age;

        println_u8(age);
    };
}

newborn = Person { age: 0 };
one_year_old = Person { age: 1 };

seq {
    print_person_age(newborn);
    print_person_age(one_year_old);
};
";

        let mut additional_values = HashMap::new();

        additional_values.insert(
            String::from("__pause_for_garbage_collection"),
            AdditionalValue {
                built_in_value: Box::new(BuiltInMappedFunction::new(
                    String::from("__pause_for_garbage_collection"),
                    context.void_type().fn_type(
                        // Add an extra parameter for the worker pointer
                        &[context.ptr_type(AddressSpace::default()).into()],
                        false,
                    ),
                    __pause_for_garbage_collection as usize,
                )),

                type_: Type::Function(FunctionType {
                    parameters: Vec::new(),
                    return_type: Box::new(Type::Unit),
                }),
            },
        );

        run_phases(&context, source, &Arguments::default(), additional_values)
    });

    // We expect `__pause_for_garbage_collection` to be called twice—one for each
    // `print_person_age` call
    for i in 0..2 {
        let (mutex, conditional_variable) = &*PAUSING_FOR_GARBAGE_COLLECTION;
        let mut pausing = mutex.lock().unwrap();

        while !*pausing {
            pausing = conditional_variable.wait(pausing).unwrap();
        }

        *pausing = false;

        mmtk_pause_all_mutators(|_| {});

        let roots_work_factory = MockRootsWorkFactory::new();
        let roots_mutex = roots_work_factory.roots();

        mmtk_register_roots(roots_work_factory);

        let roots = roots_mutex.lock().unwrap();
        let unique_roots = roots.iter().collect::<HashSet<_>>();

        // Within the first call, we expect three roots:
        // 1. `newborn`
        // 2. `one_year_old`
        // 3. The closure context value for the first `print_person_age` reference
        // 4. The closure context value for the second `print_person_age` reference
        // 5. `person`
        // 6. The context parameter of `print_person_age`'s closure
        // 7. The closure context value for the `__pause_for_garbage_collection` reference
        // 8. The context parameter of `__pause_for_garbage_collection`'s closure
        //
        // Within the second call, we expect two roots (roots 1 and 4 will have gone out of scope):
        // 1. `one_year_old`
        // 2. The closure context value for the first `print_person_age` reference
        // 3. `person`
        // 4. The context parameter of `print_person_age`'s closure
        // 5. The closure context value for the `__pause_for_garbage_collection` reference
        // 6. The context parameter of `__pause_for_garbage_collection`'s closure
        assert_eq!(unique_roots.len(), if i == 0 { 8 } else { 6 });

        mmtk_resume_all_mutators();
    }

    handle.join().unwrap()?;

    Ok(())
}
