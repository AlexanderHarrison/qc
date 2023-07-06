use crate::parser::*;

mod types;
pub use types::*;

mod type_expr;
pub use type_expr::*;

pub enum TypeError {
    FnReturnMismatch,
    TypeUndefined,
    IfStatementReturnsType,
    BraceBlockReturnsType,
    IfConditionNotBool,
    ElseIfConditionNotBool,
    IfPathMismatch,
    NoElsePathWithReturnType,
    ElsePathMismatch,
    InfixOperatorMismatch,
    FunctionUsedAsVar,
    VarUndefined,
    BuiltinFieldAccess,
    FieldDoesNotExist,
    FunctionUndefined,
    TypeUsedAsFn,
    FnArgLenMismatch,
    FnArgTypeMismatch,
}

pub fn resolve_types(type_names: &TypeNames, parsed_file: &mut ParsedFile) -> Result<(), TypeError> {
    let typing_data = TypingData::new(type_names, parsed_file)?;
    let mut ctx = Context::new(&typing_data);
    ctx.push_scope();

    for f in parsed_file.fns.iter() {
        let arg_types = f.input_args.iter()
            .map(|arg| arg.arg_type)
            .collect::<Vec<Type>>()
            .into_boxed_slice();
        let binding = Binding::Function {
            arg_types,
            return_type: f.return_type
        };

        ctx.push(f.name, binding)
    }

    for f in parsed_file.fns.iter_mut() {
        ctx.push_scope();

        for arg in f.input_args.iter() {
            ctx.push(arg.name, Binding::Type(arg.arg_type))
        }

        let return_type = type_braced_expr(&mut ctx, &mut f.body)?;

        if return_type != f.return_type { 
            return Err(TypeError::FnReturnMismatch)
        }

        ctx.pop_scope();
    }

    ctx.pop_scope();

    Ok(())
}

#[derive(Clone, Debug)]
pub struct Field {
    name: StrRef,
    field_type: Type,
}

#[derive(Clone, Debug)]
pub struct StructData {
    name: StrRef,
    fields: Box<[Field]>
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub struct StructRef { index: usize }

#[derive(Copy, Clone, Debug)]
pub enum TypeVariant {
    Builtin,
    Struct(StructRef),
}

#[derive(Copy, Clone, Debug)]
pub struct TypeData {
    _name: StrRef,
    variant: TypeVariant
}

// I hate this name
#[derive(Clone, Debug)]
pub struct TypingData {
    type_data: Box<[TypeData]>,
    struct_data: Box<[StructData]>,
}

impl TypingData {
    pub fn new(type_names: &TypeNames, file: &ParsedFile) -> Result<Self, TypeError> {
        let struct_data = file.structs.iter()
            .map(|s| {
                StructData {
                    name: s.name,
                    fields: s.fields.iter()
                        .map(|f| {
                            Field {
                                name: f.name,
                                field_type: f.field_type
                            }
                        }).collect::<Vec<Field>>()
                        .into_boxed_slice()
                }
            })
            .collect::<Vec<StructData>>();
        
        let mut type_data = Vec::with_capacity(type_names.types_len());

        for type_ref in type_names.iter_type_refs() {
            let type_name = type_names.get_type_name(type_ref);

            if type_names.is_builtin_type(type_ref) {
                type_data.push(TypeData {
                    _name: type_name,
                    variant: TypeVariant::Builtin,
                })
            } else if let Some(index) = struct_data.iter().position(|s| s.name == type_name) {
                type_data.push(TypeData {
                    _name: type_name,
                    variant: TypeVariant::Struct(StructRef { index }),
                })
            } else {
                return Err(TypeError::TypeUndefined)
            }
        }

        Ok(Self {
            struct_data: struct_data.into_boxed_slice(),
            type_data: type_data.into_boxed_slice(),
        })
    }

    pub fn lookup_type(&self, type_ref: TypeRef) -> &TypeData {
        &self.type_data[type_ref.index()]
    }

    pub fn lookup_struct(&self, struct_ref: StructRef) -> &StructData {
        &self.struct_data[struct_ref.index]
    }
}

impl std::fmt::Display for TypeError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use TypeError::*;

        let s = match self {
            FnReturnMismatch         => "incorrect function return type",
            TypeUndefined            => "type is not defined",
            IfStatementReturnsType   => "if statement without 'let' should not return a value",
            BraceBlockReturnsType    => "brace block without 'let' should not return a value",
            IfConditionNotBool       => "if condition is not a bool",
            ElseIfConditionNotBool   => "else if condition is not a bool",
            IfPathMismatch           => "all if expression paths should return the same type",
            NoElsePathWithReturnType => "if expression must contain an else block",
            ElsePathMismatch         => "all if paths must return the same type",
            InfixOperatorMismatch    => "infix operators must operate on the same type",
            FunctionUsedAsVar        => "function is not a variable",
            VarUndefined             => "variable is undefined",
            BuiltinFieldAccess       => "builtin types do not contain fields",
            FieldDoesNotExist        => "field does not exist",
            FunctionUndefined        => "function is not defined",
            TypeUsedAsFn             => "a type cannot be called as a function",
            FnArgLenMismatch         => "function does not contain proper number of arguments",
            FnArgTypeMismatch        => "function argument type is incorrect",
        };

        write!(f, "{}", s)
    }
}
