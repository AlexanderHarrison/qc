#![allow(unused)]

use std::collections::HashMap;

use crate::parser::*;
use crate::layout::*;

pub type Label = u32;
pub type Register = u16;
pub type RegisterMask = u16;
pub type XMMRegister = Register;
pub type XMMRegisterMask = RegisterMask;

// calling convention for now:
// all structs are stored and passed on the stack

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum DataLocation {
    Stack(u32),
    Registers(RegisterMask),
    XMMRegisters(XMMRegisterMask),
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub struct Data {
    pub location: DataLocation,
    pub temp: bool,
}

#[derive(Copy, Clone, Debug)]
pub enum Asm {
    Label(Label),

    CmpIntEq { src1: Register, src2: Register, dest: Register },
    CmpFloatEq { src1: XMMRegister, src2: XMMRegister, dest: Register },
    CmpZero(Register),

    JumpEq(Label),
    Jump(Label),
    LoadImmInt(i64, Register),
    LoadImmFloat(f64, XMMRegister),

    AddIntReg { src1: Register, src2: Register, dest: Register },
    SubIntReg { src1: Register, src2: Register, dest: Register },
    MulIntReg { src1: Register, src2: Register, dest: Register },
    AddFloatReg { src1: XMMRegister, src2: XMMRegister, dest: XMMRegister },
    SubFloatReg { src1: XMMRegister, src2: XMMRegister, dest: XMMRegister },
    MulFloatReg { src1: XMMRegister, src2: XMMRegister, dest: XMMRegister },
}

#[derive(Clone, Debug)]
pub struct CodeSegment {
    pub label: StrRef,
    pub arg_locations: Box<[Option<DataLocation>]>,
    pub code: Box<[Asm]>,
    pub return_location: DataLocation,
}

pub struct CompileCtx<'a> {
    pub structs: &'a [ParsedStruct],
    next_label: Label,
    pub layouts: TypeLayouts,
}

struct SegmentCtx {
    code: Vec<Asm>,
    register_usage: RegisterMask,
    xmm_register_usage: RegisterMask,
    var_names: Vec<StrRef>,
    var_locs: Vec<DataLocation>,
    var_contexts: Vec<usize>,
}

impl<'a> CompileCtx<'a> {
    pub fn new(structs: &'a [ParsedStruct], layouts: TypeLayouts) -> Self {
        CompileCtx {
            structs,
            next_label: 0,
            layouts,
        }
    }

    pub fn new_label(&mut self) -> Label {
        let label = self.next_label;
        self.next_label += 1;
        label
    }
}

impl SegmentCtx {
    pub fn new() -> Self {
        SegmentCtx {
            code: Vec::new(),
            register_usage: 0,
            xmm_register_usage: 0,
            var_names: Vec::new(),
            var_locs: Vec::new(),
            var_contexts: Vec::new(),
        }
    }

    pub fn location_of(&self, var: StrRef) -> DataLocation {
        //println!("location_of {}", var);
        let i = self.var_names
            .iter()
            .rev()
            .position(|name| *name == var)
            .expect("variable not in context");
        self.var_locs[i]
    }

    pub fn push(&mut self, a: Asm) {
        self.code.push(a);
    }

    pub fn new_register(&mut self) -> Register {
        let r = self.register_usage;
        let lowest_unused = (r + 1) & !r; // bit magic
        assert!(lowest_unused != 0); // TODO
        self.register_usage |= lowest_unused;
        //println!("new_register {}", lowest_unused);
        lowest_unused 
    }

    pub fn new_xmm_register(&mut self) -> XMMRegister {
        let r = self.xmm_register_usage;
        let lowest_unused = (r + 1) & !r; // bit magic
        assert!(lowest_unused != 0); // TODO
        self.xmm_register_usage |= lowest_unused;
        //println!("new_xmm_register {}", lowest_unused);
        lowest_unused 
    }

    pub fn return_data(&mut self, data: DataLocation) {
        //println!("return_data {:?}", data);
        match data {
            DataLocation::Registers(r) => {
                assert_eq!(r & !self.register_usage, 0);
                self.register_usage ^= r;
            }
            DataLocation::XMMRegisters(r) => {
                assert_eq!(r & !self.xmm_register_usage, 0);
                self.xmm_register_usage ^= r;
            }
            DataLocation::Stack(_) => unimplemented!(),
        }
    }

    pub fn name_location(&mut self, name: StrRef, data_loc: DataLocation) {
        //println!("name_location {}, {:?}", name, data_loc);
        match data_loc {
            DataLocation::Registers(r) => {
                assert_eq!(r & !self.register_usage, 0);
            },
            DataLocation::XMMRegisters(r) => {
                assert_eq!(r & !self.xmm_register_usage, 0);
            },
            DataLocation::Stack(size) => unimplemented!(),
        };

        self.var_names.push(name);
        self.var_locs.push(data_loc);
    }

    pub fn start_context(&mut self) {
        //println!("start_context");
        self.var_contexts.push(self.var_names.len());
    }

    pub fn end_context(&mut self) {
        //println!("end_context");
        let new_var_count: usize = self.var_contexts.pop().unwrap();
        self.var_names.truncate(new_var_count);
        
        let removed = self.var_locs[new_var_count..]
            .iter()
            .rev()
            .copied()
            .collect::<Vec<DataLocation>>();
        self.var_locs.truncate(new_var_count);

        for loc in removed {
            self.return_data(loc);
        }
    }
}

pub fn compile(file: &ParsedFile, layouts: TypeLayouts) {
    let mut c_ctx = CompileCtx::new(&file.structs, layouts);
    let mut segments = Vec::new();
    
    for f in file.fns.iter() {
        segments.push(compile_segment(&mut c_ctx, f));
    }

    todo!();
}

pub fn compile_segment(c_ctx: &mut CompileCtx, f: &ParsedFn) -> CodeSegment {
    let mut s_ctx = SegmentCtx::new();

    s_ctx.start_context();

    let mut arg_locations = Vec::with_capacity(f.input_args.len());
    for arg in f.input_args.iter() {
        let layout = c_ctx.layouts.get_layout(arg.arg_type.type_ref);

        if let Some(layout) = layout {
            let arg_location = match layout.pass_repr {
                PassRepr::Register => DataLocation::Registers(s_ctx.new_register()),
                PassRepr::XMMRegister => DataLocation::Registers(s_ctx.new_xmm_register()),
                PassRepr::Stack => unimplemented!(),
            };
            s_ctx.name_location(arg.name, arg_location);
            arg_locations.push(Some(arg_location));
        } else {
            arg_locations.push(None);
        }
    }

    let label = c_ctx.new_label();
    s_ctx.push(Asm::Label(label));
    let return_data = compile_braced_expr(c_ctx, &mut s_ctx, &f.body).unwrap();

    s_ctx.end_context();

    CodeSegment {
        label: f.name,
        code: s_ctx.code.into_boxed_slice(),
        arg_locations: arg_locations.into_boxed_slice(),
        return_location: return_data.location,
    }
}

fn compile_braced_expr(c_ctx: &mut CompileCtx, s_ctx: &mut SegmentCtx, expr: &ParsedBracedExpr) -> Option<Data> {
    s_ctx.start_context();
    for stmt in expr.statements.iter() {
        compile_statement(c_ctx, s_ctx, stmt);
    }

    if let Some(ref ret_expr) = expr.return_expr {
        let ret_loc = compile_expr(c_ctx, s_ctx, ret_expr);
        s_ctx.end_context();
        Some(ret_loc)
    } else {
        s_ctx.end_context();
        None
    }
}

fn compile_statement(c_ctx: &mut CompileCtx, s_ctx: &mut SegmentCtx, stmt: &ParsedStatement) {
    match stmt {
        ParsedStatement::Let { name, expr } => {
            let data = compile_expr(c_ctx, s_ctx, expr);
            s_ctx.name_location(*name, data.location);
        },
        ParsedStatement::Return { .. } => unimplemented!(),
        ParsedStatement::If { if_statement } => {
            let ret = compile_if(c_ctx, s_ctx, if_statement);
            assert_eq!(ret, None); // should already be checked by type checker
        },
        ParsedStatement::SubBracedExpr { expr } => {
            let ret = compile_braced_expr(c_ctx, s_ctx, expr);
            assert_eq!(ret, None); // should already be checked by type checker
        }
    }
}

fn compile_if(c_ctx: &mut CompileCtx, s_ctx: &mut SegmentCtx, stmt: &ParsedIfStatement) -> Option<Data> {
    let mut path_label = None;

    let exit_label = c_ctx.new_label();

    for path in stmt.paths.iter() {
        if let Some(label) = path_label.take() {
            s_ctx.push(Asm::Label(label));
        }

        let cond_loc = compile_expr(c_ctx, s_ctx, &path.condition);
        let reg = match cond_loc.location {
            DataLocation::Registers(r) => r,
            _ => panic!("probably should not be returning a bool on the stack"),
        };

        s_ctx.push(Asm::CmpZero(reg));
        if cond_loc.temp { s_ctx.return_data(cond_loc.location) }

        let else_label = c_ctx.new_label();
        s_ctx.push(Asm::JumpEq(else_label));
        path_label = Some(else_label);

        let ret_loc = compile_braced_expr(c_ctx, s_ctx, &path.expr_true);
        if ret_loc != None { unimplemented!(); }
        s_ctx.push(Asm::Jump(exit_label));
    }

    s_ctx.push(Asm::Label(path_label.unwrap()));
    let ret_loc = if let Some(ref else_expr) = stmt.else_expr {
        compile_braced_expr(c_ctx, s_ctx, else_expr)
    } else {
        None
    };
    s_ctx.push(Asm::Label(exit_label));
    ret_loc
}

fn compile_expr(c_ctx: &mut CompileCtx, s_ctx: &mut SegmentCtx, stmt: &ParsedExpr) -> Data {
    let mut simple_expr_iter = stmt.simple_exprs.iter();
    let mut operators = stmt.operators.iter();

    let mut res_loc = compile_simple_expr(c_ctx, s_ctx, simple_expr_iter.next().unwrap());

    while let Some(simple_expr) = simple_expr_iter.next() {
        let rhs_loc = compile_simple_expr(c_ctx, s_ctx, simple_expr);
        let operator = operators.next().unwrap();
        
        res_loc = compile_operator(c_ctx, s_ctx, res_loc, rhs_loc, *operator);
    }

    res_loc
}

fn compile_operator(
    c_ctx: &mut CompileCtx, 
    s_ctx: &mut SegmentCtx,
    lhs: Data,
    rhs: Data,
    op: InfixOperator
) -> Data {
    match (lhs.location, rhs.location) {
        (DataLocation::Registers(src1), DataLocation::Registers(src2)) => {
            let dest = if lhs.temp {
                if rhs.temp { s_ctx.return_data(rhs.location) }
                src1
            } else if rhs.temp {
                src2
            } else {
                s_ctx.new_register()
            };

            match op {
                InfixOperator::Add => s_ctx.push(Asm::AddIntReg { src1, src2, dest }),
                InfixOperator::Subtract => s_ctx.push(Asm::SubIntReg { src1, src2, dest }),
                InfixOperator::Multiply => s_ctx.push(Asm::MulIntReg { src1, src2, dest }),
                InfixOperator::Equals => todo!(),
            }

            Data { location: DataLocation::Registers(dest), temp: true }
        },
        (DataLocation::XMMRegisters(src1), DataLocation::XMMRegisters(src2)) => {
            let dest = if lhs.temp {
                if rhs.temp { s_ctx.return_data(rhs.location) }
                src1
            } else if rhs.temp {
                src2
            } else {
                s_ctx.new_xmm_register()
            };

            match op {
                InfixOperator::Add => s_ctx.push(Asm::AddFloatReg { src1, src2, dest: src1 }),
                InfixOperator::Subtract => s_ctx.push(Asm::SubFloatReg { src1, src2, dest: src1 }),
                InfixOperator::Multiply => s_ctx.push(Asm::MulFloatReg { src1, src2, dest: src1 }),
                InfixOperator::Equals => todo!(),
            }

            Data { location: DataLocation::XMMRegisters(dest), temp: true }
        },
        _ => panic!("todo!")
    }
}

fn compile_simple_expr(c_ctx: &mut CompileCtx, s_ctx: &mut SegmentCtx, expr: &ParsedSimpleExpr) -> Data {
    let simple_loc = match expr.expr_type {
        SimpleExprType::BracedExpr(ref braced_expr) => compile_braced_expr(c_ctx, s_ctx, braced_expr).unwrap(),
        SimpleExprType::InlineExpr(ref atom) => compile_atom(c_ctx, s_ctx, atom),
    };

    if !expr.accesses.is_empty() {
        unimplemented!();
    }

    simple_loc
}

fn compile_atom(c_ctx: &mut CompileCtx, s_ctx: &mut SegmentCtx, atom: &ParsedAtom) -> Data {
    match atom {
        ParsedAtom::FunctionCall { name, arguments, ret_type } => todo!(),
        ParsedAtom::Variable { name } => Data { location: s_ctx.location_of(*name), temp: false },
        ParsedAtom::If { if_statement } => compile_if(c_ctx, s_ctx, if_statement).unwrap(),
        ParsedAtom::Unit => panic!("idk what to do here"),
        ParsedAtom::IntegerLiteral(n) => {
            let reg = s_ctx.new_register();
            s_ctx.push(Asm::LoadImmInt(*n, reg));
            let location = DataLocation::Registers(reg);
            Data { location, temp: true }
        },
        ParsedAtom::FloatLiteral(f) => {
            let reg = s_ctx.new_xmm_register();
            s_ctx.push(Asm::LoadImmFloat(*f, reg));
            let location = DataLocation::XMMRegisters(reg);
            Data { location, temp: true }
        },
    }
}

pub fn reg_idx(reg: &Register) -> u16 {
    let mut reg = *reg;
    let mut i = 0;
    reg >>= 1;
    while reg != 0 {
        reg >>= 1;
        i += 1;
    }
    i
}

impl std::fmt::Display for Asm {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Asm::Label(l) => write!(f, "l{}:", l),
            Asm::CmpZero(r) => write!(f, "\tcmpz r{}", reg_idx(r)),
            Asm::JumpEq(l) => write!(f, "\tjumpeq l{}", l),
            Asm::Jump(l) => write!(f, "\tjump l{}", l),
            Asm::LoadImmInt(n, r) => write!(f, "\tload r{} {}", reg_idx(r), n),
            Asm::LoadImmFloat(n, r) => write!(f, "\tload x{} {}", reg_idx(r), n),
            Asm::AddIntReg { src1, src2, dest } => write!(f, "\tadd r{} r{} r{}", reg_idx(dest), reg_idx(src1), reg_idx(src2)),
            Asm::MulIntReg { src1, src2, dest } => write!(f, "\tmul r{} r{} r{}", reg_idx(dest), reg_idx(src1), reg_idx(src2)),
            Asm::SubIntReg { src1, src2, dest } => write!(f, "\tsub r{} r{} r{}", reg_idx(dest), reg_idx(src1), reg_idx(src2)),
            Asm::AddFloatReg { src1, src2, dest } => write!(f, "\tadd x{} x{} x{}", reg_idx(dest), reg_idx(src1), reg_idx(src2)),
            Asm::MulFloatReg { src1, src2, dest } => write!(f, "\tmul x{} x{} x{}", reg_idx(dest), reg_idx(src1), reg_idx(src2)),
            Asm::SubFloatReg { src1, src2, dest } => write!(f, "\tsub x{} x{} x{}", reg_idx(dest), reg_idx(src1), reg_idx(src2)),
            _ => todo!(),
        }
    }
}
