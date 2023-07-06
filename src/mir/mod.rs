#![allow(unused)]

use crate::parser::*;
use crate::typing::*;

use std::collections::HashMap;

// should contain exactly the same information as before,
// only simplified. 
// I.e., don't merge enums into structs, but combine expression types.
//
// Reduce number of types to as little as possible
// Only need to change functions.

pub struct MIRFile {
    pub fns: Box<[MIRFn]>
}

pub struct MIRFn {
    pub name: StrRef,
    pub input_args: Box<[ParsedArgument]>,
    pub return_type: Type,
    pub body: MIRExprGraph,
}

pub struct MIRIfPath {
    pub condition: MIRExprNodeRef,
    pub expr_true: 
}

#[derive(Copy, Clone, Debug)]
pub enum MIROperation {
    Fn { 
        name: StrRef,
        arg_start: u32,
        args_len: u32
    },
    Infix {
        operator: InfixOperator,
        arg_start: u32,
        args_len: u32,
    },
    If {
        paths: Box<[MIRIfPath]>,
        else_expr: Option<>
    }
    ConstantFloat(f64),
    ConstantInteger(i64),
}

// represents a single operation (for now)
#[derive(Clone, Debug)]
pub struct MIRExprNode {
    result_type: Type,
    operation: MIROperation,
}

pub struct MIRExprGraph {
    exprs: Box<[MIRExprNode]>,
    args: Box<[MIRExprNodeRef]>,
}

pub type MIRExprNodeRef = u32;

struct MIRExprGraphBuilder {
    exprs: Vec<MIRExprNode>,
    args: Vec<MIRExprNodeRef>,
    expr_names: HashMap<StrRef, MIRExprNodeRef>,
}

pub fn convert_file_to_mir(parsed_file: &ParsedFile) -> MIRFile {
    let fns = parsed_file.fns.iter().map(convert_fn_to_mir).collect::<Vec<_>>();
    MIRFile {
        fns: fns.into_boxed_slice(),
    }
}

fn mir_convert_fn(f: &ParsedFn) -> MIRFn {
    let mut mir_builder = MIRExprGraphBuilder::new();
    
    for (i, arg) in f.input_args.iter().enumerate() {
        let op = MIROperation::Argument { index: i },
        mir_builder.add_node_named(arg.arg_type, op, &[], arg.name)
    }

    mir_convert_braced(&mut exprs, &f.body);

    MIRFn {
        name: f.name,
        input_args: f.input_args.clone(),
        return_type: f.return_type,
        body: mir_builder.build(),
    }
}

fn mir_convert_braced(mir_builder: &mut MIRExprGraphBuilder, expr: &ParsedBracedExpr) -> Option<MIRExprNodeRef> {
    for statement in expr.statements.iter() {
        mir_convert_statement(mir_builder, statement)
    }

    if let Some(ret_expr) = expr.return_expr {
        Some(mir_convert_expr(mir_builder, &*ret_expr, None))
    } else {
        None
    }
}

fn mir_convert_statement(mir_builder: &mut MIRExprGraphBuilder, statement: &ParsedStatement) {
    match statement {
        ParsedStatement::Let { name, expr } => {
            let idx = mir_convert_expr(mir_builder, expr);
            mir_builder.name_node(idx, name);
        }
        ParsedStatement::Return { .. } => unimplemented!(),
        ParsedStatement::If { if_statement } => {
            todo!()
        }
        ParsedStatement::SubBracedExpr { expr } => {
            mir_convert_braced(mir_builder, expr);
        }
    }
}

fn mir_convert_expr(mir_builder: &mut MIRExprGraphBuilder, expr: &ParsedExpr) -> MIRExprNodeRef {
    todo!()
}

impl MIRExprGraphBuilder {
    pub fn new() -> Self {
        MIRExprGraphBuilder {
            exprs: Vec::new(),
            args: Vec::new(),
            expr_names: HashMap::new(),
        }
    }

    pub fn build(self) -> MIRExprGraph {
        MIRExprGraph {
            exprs: self.exprs.into_boxed_slice(),
            args: self.args.into_boxed_slice(),
        }
    }

    pub fn add_node(&mut self, ty: Type, op: MIROperation, args: &[StrRef]) -> MIRExprNodeRef {
        let args_start = self.args.len();

        for arg in args {
            let idx = self.expr_names[a];
            self.args.push(idx);
        }

        let node = MIRExprNode {
            result_type: ty,
            operation: op,
            args_len: args.len(),
            args_start,
        };

        let idx = self.exprs.len();
        self.exprs.push(node);
        idx
    }

    pub fn name_node(&mut self, node_idx: MIRExprNodeRef, name: StrRef) {
        self.expr_ames.insert(name, node_idx);
    }
}
