use crate::parser::*;

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum PassRepr {
    Register,
    XMMRegister,
    Stack,
}

#[derive(Clone, Debug)]
pub struct TypeLayout {
    pub pass_repr: PassRepr,
    pub size: u32,
    pub field_offsets: Option<Box<[u32]>>
}

pub struct TypeLayouts {
    pub layouts: Box<[Option<TypeLayout>]>, // zero sized types have no layout
}

pub fn calculate_layouts(_structs: &[ParsedStruct], names: &TypeNames) -> TypeLayouts {
    assert!(TypeNames::BUILTIN_TYPE_COUNT == 4);

    let mut layouts = vec![
        None, // ()
        Some(TypeLayout { // I64
            pass_repr: PassRepr::Register,
            size: 8,
            field_offsets: None,
        }),
        Some(TypeLayout { // F64
            pass_repr: PassRepr::XMMRegister,
            size: 8,
            field_offsets: None,
        }),
        Some(TypeLayout { // Bool
            pass_repr: PassRepr::Register,
            size: 1,
            field_offsets: None,
        }),
    ];

    // TODO other types

    TypeLayouts {
        layouts: layouts.into_boxed_slice(),
    }
}

impl TypeLayouts {
    pub fn get_layout(&self, type_ref: TypeRef) -> &Option<TypeLayout> {
        &self.layouts[type_ref.index()]
    }
}
