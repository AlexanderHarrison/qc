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
pub enum MIRInfixOperation {
    Add { lhs: MIRExprNodeRef, rhs: MIRExprNodeRef },
    Sub { lhs: MIRExprNodeRef, rhs: MIRExprNodeRef },
    Mul { lhs: MIRExprNodeRef, rhs: MIRExprNodeRef },
}

#[derive(Copy, Clone, Debug)]
pub enum MIROperation {
    Argument { index: u32 },
    Fn { 
        name: StrRef,
        arg_start: u32,
        args_len: u32,
    },
    InfixOp(MIRInfixOperation),
    If {
        condition: MIRExprNodeRef,
        expr_true: MIRExprNodeRef,
        expr_false: Option<MIRExprNodeRef>,
    },
    ConstantFloat(f64),
    ConstantInteger(i64),
}

#[derive(Clone, Debug)]
pub struct MIRExprNode {
    result_type: Type,
    operation: MIROperation,
}

pub struct MIRExprGraph {
    exprs: Box<[MIRExprNode]>,
    args: Box<[MIRExprNodeRef]>,
    out_node: Option<MIRExprNodeRef>,
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
    let mut mir_graph = MIRExprGraphBuilder::new();
    
    for (i, arg) in f.input_args.iter().enumerate() {
        let op = MIROperation::Argument { index: i },
        let node = mir_graph.add_node(arg.arg_type, op);
        mir_graph.name_node(node, arg.name);
    }

    let out_node = mir_convert_braced(&mut mir_graph, &f.body);

    MIRFn {
        name: f.name,
        input_args: f.input_args.clone(),
        return_type: f.return_type,
        body: mir_builder.build(out_node),
    }
}

fn mir_convert_braced(mir_graph: &mut MIRExprGraphBuilder, expr: &ParsedBracedExpr) -> Option<MIRExprNodeRef> {
    for statement in expr.statements.iter() {
        mir_convert_statement(mir_graph, statement)
    }

    if let Some(ret_expr) = expr.return_expr {
        Some(mir_convert_expr(mir_graph, &*ret_expr, None))
    } else {
        None
    }
}

fn mir_convert_statement(mir_graph: &mut MIRExprGraphBuilder, statement: &ParsedStatement) {
    match statement {
        ParsedStatement::Let { name, expr } => {
            let idx = mir_convert_expr(mir_graph, expr);
            mir_graph.name_node(idx, name);
        }
        ParsedStatement::Return { .. } => unimplemented!(),
        ParsedStatement::If { if_statement } => {
            mir_convert_if(mir_graph, if_statement);
        }
        ParsedStatement::SubBracedExpr { expr } => {
            mir_convert_braced(mir_graph, expr);
        }
    }
}

fn mir_convert_if(mir_graph: &mut MIRExprGraphBuilder, statement: &ParsedIfStatement) -> Option<MIRExprNodeRef> {
    let if_op = MIROperation::If {
        condition: mir_convert_expr(mir_graph, &path.condition),
        expr_true: mir_convert_braced(mir_graph, &path.expr_true),
        expr_false: None, // will be set when we process the next path
    };

    let node_type = mir_graph.get_node(if_op.expr_true).result_type;
    let init_path_ref = mir_graph.add_node(node_type, if_op);
    let mut prev_path_ref = init_path_ref;

    for path in statement.paths[1..].iter() {
        let expr_true = mir_convert_braced(mir_graph, &path.expr_true);
        let node_type = mir_graph.get_node(expr_true).result_type;
        let if_op = MIROperation::If {
            condition: mir_convert_expr(mir_graph, &path.condition),
            expr_true,
            expr_false: None, // will be set when we process the next path
        };

        let path_ref = mir_graph.add_node(node_type, if_op);

        match mir_graph.get_node(prev_path_ref).operation {
            MIROperation::If { .., ref mut expr_false } => *expr_false = Some(path_ref),
            _ => unreachable!(),
        }

        prev_path_ref = path_ref;
    }

    if let Some(ref else_expr) = statement.else_expr {
        let else_ref = mir_convert_braced(mir_graph, else_expr);

        match mir_graph.get_node(prev_path_ref).operation {
           MIROperation::If { .., ref mut expr_false } => *expr_false = else_ref,
           _ => unreachable!(),
        }
    }

    init_path_ref
}

fn mir_convert_expr(mir_graph: &mut MIRExprGraphBuilder, expr: &ParsedExpr) -> MIRExprNodeRef {
    let mut lhs = mir_convert_simple_expr(mir_graph, &expr.simple_exprs[0]);

    for i in 0..expr.operators.len() {
        let simple_expr = &expr.simple_exprs[i];
        let infix_op = expr.operators[i];
        let rhs = mir_convert_simple_expr(mir_graph, simple_expr);

        let infix = match infix_op {
            InfixOperator::Add => MIRInfixOperation::Add { lhs, rhs },
            InfixOperator::Subtract => MIRInfixOperation::Sub { lhs, rhs },
            InfixOperator::Multiply => MIRInfixOperation::Mul { lhs, rhs },
            InfixOperator::Equals => todo!(),
        };
        let op = MIROperation::InfixOp(infix);
        lhs = mir_graph.add_node(expr.ret_type.unwrap(), op);
    }

    lhs
}

fn mir_convert_simple_expr(mir_graph: &mut MIRExprGraphBuilder, expr: &ParsedSimpleExpr) -> MIRExprNodeRef {
    let simple_expr_ref = match expr.expr_type {
        ParsedSimpleExprType::BracedExpr(ref expr) => mir_convert_braced(mir_graph, expr).unwrap(),
        ParsedSimpleExprType::InlineExpr(ref atom) => mir_convert_atom(mir_graph, atom),
    }

    let last_ref = simple_expr_ref;

    for access in expr.accesses.iter() {
        unimplemented!()
    }

    last_ref
}

fn mir_convert_atom(mir_graph: &mut MIRExprGraphBuilder, atom: &ParsedAtom) -> MIRExprNodeRef {
    match atom {
        ParsedAtom::FunctionCall { name, arguments, ret_type } => {
            let (arg_start, args_len) = mir_graph.add_args(mir_convert_expr, arguments.iter());
            let op = MIROperation::Fn { name: *name, arg_start, args_len };
            mir_graph.add_node(ret_type.unwrap(), op)
        }
        ParsedAtom::Variable { name } => mir_graph.get_node_by_name(*name),
        ParsedAtom::If { if_statement } => mir_convert_if(mir_graph, if_statement),
        ParsedAtom::Unit => unimplemented!(),
        ParsedAtom::IntegerLiteral(n) => {
            let op = MIROperation::ConstantInteger(*n);
            mir_graph.add_node(Type::from_ref(INT_TYPE), op)
        }
        ParsedAtom::FloatLiteral(n) => {
            let op = MIROperation::ConstantFloat(*n);
            mir_graph.add_node(Type::from_ref(FLOAT_TYPE), op)
        }
    }
}

impl MIRExprGraphBuilder {
    pub fn new() -> Self {
        MIRExprGraphBuilder {
            exprs: Vec::new(),
            args: Vec::new(),
            expr_names: HashMap::new(),
        }
    }

    pub fn build(self, out_node: Option<MIRExprNodeRef>) -> MIRExprGraph {
        MIRExprGraph {
            exprs: self.exprs.into_boxed_slice(),
            args: self.args.into_boxed_slice(),
            out_node,
        }
    }

    pub fn get_node(&mut self, node: MIRExprNodeRef) -> &mut MIRExprNode {
        &mut self.exprs[node as usize]
    }

    pub fn get_node_by_name(&mut self, name: StrRef) -> MIRExprNodeRef {
        self.expr_names[&name]
    }

    pub fn add_node(&mut self, ty: Type, op: MIROperation) -> MIRExprNodeRef {
        let node = MIRExprNode {
            result_type: ty,
            operation: op,
        };

        let idx = self.exprs.len();
        self.exprs.push(node);
        idx as u32
    }

    // returns (args_start, args_len)
    pub fn add_args<T, F, I>(&mut self, f: F, args: I) -> (u32, u32) 
    where
        F: FnMut(&mut Self, T) -> MIRExprNodeRef,
        I: Iterator<Item=T>,
    {
        let args_start = self.args.len();
        let mut args_len = 0;
        for arg in args {
            args_len += 1;
            let node_ref = f(self, arg);
            self.args.push(node_ref);
        }

        (args_start as u32, args_len)
    }

    pub fn name_node(&mut self, node_idx: MIRExprNodeRef, name: StrRef) {
        self.expr_names.insert(name, node_idx);
    }
}
