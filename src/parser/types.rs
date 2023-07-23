use super::*;
use crate::lexer::Token;
use crate::typing::Type;

#[derive(Debug)]
pub struct ParsedFile {
    pub fns: Box<[ParsedFn]>,
    pub structs: Box<[ParsedStruct]>,
}

#[derive(Debug)]
pub struct ParsedFn {
    pub name: StrRef,
    pub input_args: Box<[ParsedArgument]>,
    pub return_type: Type,
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
    pub field_type: Type,
}

#[derive(Debug)]
pub struct ParsedArgument {
    pub name: StrRef,
    pub arg_type: Type,
}

#[derive(Copy, Clone, Debug)]
pub enum ReferenceFlags {
    Mutable = 1 << 0,
    Nullable = 1 << 1,
    Unique = 1 << 2,
}

#[derive(Copy, Clone, Debug, Default, PartialEq, Eq)]
pub struct ParsedReference {
    pub flags: u8,
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub struct References(pub tinyvec::ArrayVec<[ParsedReference; 14]>);

impl References {
    pub fn new() -> Self {
        References(tinyvec::ArrayVec::new())
    }

    pub fn push(&mut self, reference: ParsedReference) {
        if self.0.len() == 14 {
            panic!("type cannot have more than 14 references")
        }

        self.0.push(reference)
    }
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

    pub fn result_type(self, expr_type: Type) -> Type {
        match self {
            InfixOperator::Add => expr_type,
            InfixOperator::Subtract => expr_type,
            InfixOperator::Multiply => expr_type,
            InfixOperator::Equals => Type::from_ref(BOOL_TYPE),
        }
    }
}

/// Includes infix operators
/// such as 1 + { let x = 2; x } * ( 3 + 4 )
#[derive(Debug)]
pub struct ParsedExpr {
    pub simple_exprs: Box<[ParsedSimpleExpr]>,
    pub operators: Box<[InfixOperator]>,
    pub ret_type: Option<Type>,
}

/// No infix operators, allows fields and methods.
/// such as a.test, (1 + 2).test(), var.field().test
#[derive(Debug)]
pub struct ParsedSimpleExpr {
    pub expr_type: SimpleExprType,
    pub accesses: Box<[ParsedExprAccess]>,
    pub ret_type: Option<Type>,
}

/// field or method access
#[derive(Debug)]
pub enum ParsedExprAccess {
    Field {
        name: StrRef,
    },
    Method {
        name: StrRef,
        arguments: Box<[ParsedExpr]>,
    }
}

/// No infix operators.
/// such as 1, (1 + 2), { let x = 2; x }, add(1, 2)
#[derive(Debug)]
pub enum SimpleExprType {
    BracedExpr(ParsedBracedExpr),
    InlineExpr(ParsedAtom)
}

/// such as { let x = 2; x }
#[derive(Debug)]
pub struct ParsedBracedExpr {
    pub statements: Box<[ParsedStatement]>,
    pub return_expr: Option<Box<ParsedExpr>>,
}

/// such as 1, (2 + 4), a, add(1, 2), s.field
#[derive(Debug)]
pub enum ParsedAtom {
    FunctionCall {
        name: StrRef,
        arguments: Box<[ParsedExpr]>,
        ret_type: Option<Type>,
    },
    Variable {
        name: StrRef,
    },
    If {
        if_statement: ParsedIfStatement
    },
    Unit,
    IntegerLiteral(i64),
    FloatLiteral(f64),
}

#[derive(Debug)]
pub struct ParsedIfStatement {
    pub paths: Box<[IfPath]>,
    pub else_expr: Option<ParsedBracedExpr>,
}

#[derive(Debug)]
pub struct IfPath {
    pub condition: ParsedExpr,
    pub expr_true: ParsedBracedExpr,
}

#[derive(Debug)]
pub enum ParsedStatement {
    Let {
        name: StrRef,
        expr: ParsedExpr,
    },
    Return {
        expr: ParsedExpr,
    },
    If {
        if_statement: ParsedIfStatement
    },
    SubBracedExpr {
        expr: ParsedBracedExpr,
    }
}
