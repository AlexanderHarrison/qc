use super::*;
use crate::lexer::Token;

#[derive(Debug)]
pub struct TypeData { 
    type_string: StrRef,
}

impl TypeData {
    pub fn new(type_string: StrRef) -> Self {
        Self { type_string }
    }
}

impl std::cmp::PartialEq<&str> for TypeData {
    fn eq(&self, other: &&str) -> bool { self.type_string == *other }
}
impl std::cmp::PartialEq<str> for TypeData {
    fn eq(&self, other: &str) -> bool { self.type_string == *other }
}

#[derive(Debug)]
pub struct ParsedFile {
    pub fns: Box<[ParsedFn]>,
    pub structs: Box<[ParsedStruct]>,
}

#[derive(Debug)]
pub struct ParsedFn {
    pub name: StrRef,
    pub input_args: Box<[ParsedArgument]>,
    pub return_type: ParsedType,
    pub body: ParsedBracedExpr,
}

#[derive(Debug)]
pub struct ParsedStruct {
    pub name: StrRef,
    pub fields: Box<[ParsedField]>,
}

#[derive(Debug)]
pub struct ParsedField {
    pub name: StrRef,
    pub field_type: ParsedType,
}

#[derive(Debug)]
pub struct ParsedArgument {
    pub name: StrRef,
    pub arg_type: ParsedType,
}

#[derive(Debug)]
pub struct ParsedType {
    pub type_ref: TypeRef,
    pub references: Box<[ParsedReference]>
}

#[derive(Copy, Clone, Debug)]
pub enum ReferenceFlags {
    Mutable = 1 << 0,
    Nullable = 1 << 1,
    Unique = 1 << 2,
}

#[derive(Copy, Clone, Debug)]
pub struct ParsedReference {
    pub flags: u8,
}

#[derive(Copy, Clone, Debug)]
pub enum InfixOperator {
    Add,
    Subtract,
    Multiply,
    Equals,
}

impl InfixOperator {
    pub fn from_token(token: Token) -> Option<Self> {
        Some(match token {
            Token::Plus => InfixOperator::Add,
            Token::Minus => InfixOperator::Subtract,
            Token::Asterisk => InfixOperator::Multiply,
            Token::DoubleEquals => InfixOperator::Equals,
            _ => return None
        })
    }
}

/// Includes infix operators
/// such as 1 + { let x = 2; x } * ( 3 + 4 )
#[derive(Debug)]
pub struct ParsedExpr {
    pub simple_exprs: Box<[ParsedSimpleExpr]>,
    pub operators: Box<[InfixOperator]>,
}

/// No infix operators.
/// such as 1, (1 + 2), { let x = 2; x }, add(1, 2)
#[derive(Debug)]
pub enum ParsedSimpleExpr {
    BracedExpr(ParsedBracedExpr),
    InlineExpr(ParsedAtom)
}

/// such as { let x = 2; x }
#[derive(Debug)]
pub struct ParsedBracedExpr {
    pub statements: Box<[ParsedStatement]>,
    pub return_expr: Option<Box<ParsedExpr>>,
}

/// such as 1, (2 + 4), add(1, 2)
#[derive(Debug)]
pub enum ParsedAtom {
    FunctionCall {
        name: StrRef,
        arguments: Box<[ParsedExpr]>,
    },
    Variable {
        name: StrRef,
    },
    Unit,
    IntegerLiteral(isize),
    FloatLiteral(f64),
}

#[derive(Debug)]
pub enum ParsedStatement {
    Let {
        name: StrRef,
        expr: ParsedExpr,
    },
    Return {
        expr: ParsedExpr,
    }
}
