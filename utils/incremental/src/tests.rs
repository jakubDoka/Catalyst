use crate as incremental;
use crate::Deserializer;

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub struct Something {
    field: u8,
    another_field: usize,
}

crate::impl_default_increment!(Something);

#[derive(proc_macros::Increment, PartialEq, Eq, Debug)]
pub struct SomethingElse {
    vec: Vec<u8>,
    another_vec: Vec<Something>,
    unit: Unit,
    tuple: Tuple<u8>,
}

#[derive(proc_macros::Increment, PartialEq, Eq, Debug)]
struct Unit;

#[derive(proc_macros::Increment, PartialEq, Eq, Debug)]
struct Tuple<T>(T, u8);

#[test]
fn serialize_deserialize() {
    let mut serializer = crate::Serializer::default();

    let something = Something {
        field: 5,
        another_field: 10,
    };

    let something_else = SomethingElse {
        vec: vec![1, 2, 3, 4, 5],
        another_vec: vec![something, something],
        unit: Unit,
        tuple: Tuple(5, 10),
    };

    let serialized = serializer.serialize(&1, &something_else);
    let deserialized: SomethingElse =
        unsafe { Deserializer::new(&serialized).deserialize(&1).unwrap() };

    assert_eq!(something_else, deserialized);
}
