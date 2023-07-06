#![allow(unused)]

use super::*;

pub fn type_braced_expr(ctx: &mut Context, expr: &mut ParsedBracedExpr) -> Result<Type, TypeError> {
    ctx.push_scope();

    for statement in expr.statements.iter_mut() {
        match statement {
            ParsedStatement::Let { name, expr } => {
                let let_type = type_expr(ctx, expr)?;
                ctx.push(*name, Binding::Type(let_type));
            }
            ParsedStatement::Return { expr } => {
                unimplemented!(); // TODO somehow typecheck
                //let return_type = type_expr(ctx, expr)?;
                //assert_unit_type(ctx, return_type).map_err(|_| TypeError::)?;
            }
            ParsedStatement::If { if_statement } => {
                let return_type = type_if_expression(ctx, if_statement)?;
                if return_type != Type::from_ref(UNIT_TYPE) { 
                    return Err(TypeError::IfStatementReturnsType) 
                }
            }
            ParsedStatement::SubBracedExpr { expr } => {
                let return_type = type_braced_expr(ctx, expr)?;
                if return_type != Type::from_ref(UNIT_TYPE) { 
                    return Err(TypeError::BraceBlockReturnsType) 
                }
            }
        }
    }

    let return_type = match expr.return_expr {
        Some(ref mut expr) => type_expr(ctx, expr)?,
        None => Type::from_ref(UNIT_TYPE),
    };

    ctx.pop_scope();

    Ok(return_type)
}

fn type_if_expression(ctx: &mut Context, if_statement: &mut ParsedIfStatement) -> Result<Type, TypeError> {
    let bool_type = Type::from_ref(BOOL_TYPE);

    let condition_type = type_expr(ctx, &mut if_statement.paths[0].condition)?;
    if condition_type != bool_type { 
        return Err(TypeError::IfConditionNotBool) 
    }

    let return_type = type_braced_expr(ctx, &mut if_statement.paths[0].expr_true)?;

    for path in if_statement.paths[1..].iter_mut() {
        let condition_type = type_expr(ctx, &mut path.condition)?;
        if condition_type != bool_type { 
            return Err(TypeError::ElseIfConditionNotBool) 
        }

        let path_return_type = type_braced_expr(ctx, &mut path.expr_true)?;
        if path_return_type != return_type {
            return Err(TypeError::IfPathMismatch)
        }
    }

    let else_type = match if_statement.else_expr {
        Some(ref mut else_expr) => { type_braced_expr(ctx, else_expr)? },
        None if return_type != Type::from_ref(UNIT_TYPE) => {
            return Err(TypeError::NoElsePathWithReturnType)
        }
        None => Type::from_ref(UNIT_TYPE),
    };

    if return_type != else_type {
        return Err(TypeError::ElsePathMismatch)
    }

    Ok(return_type)
}

// TODO operator precedence
fn type_expr(ctx: &mut Context, expr: &mut ParsedExpr) -> Result<Type, TypeError> {
    if expr.simple_exprs.is_empty() {
        return Ok(Type::from_ref(UNIT_TYPE))
    }

    let mut lhs_type = type_simple_expr(ctx, &mut expr.simple_exprs[0])?;

    for i in 1..expr.simple_exprs.len() {
        let rhs_type = type_simple_expr(ctx, &mut expr.simple_exprs[i])?;
        let infix_operator = expr.operators[i-1];
        if lhs_type != rhs_type { return Err(TypeError::InfixOperatorMismatch) }

        lhs_type = infix_operator.result_type(lhs_type);
    }

    expr.ret_type = Some(lhs_type);
    Ok(lhs_type)
}

fn type_simple_expr(ctx: &mut Context, expr: &mut ParsedSimpleExpr) -> Result<Type, TypeError> {
    let mut expr_type = match &mut expr.expr_type {
        SimpleExprType::InlineExpr(atom) => {
            match atom {
                ParsedAtom::FunctionCall { name, arguments } => {
                    type_function_call(ctx, *name, arguments)?
                }
                ParsedAtom::Variable { name } => {
                    match ctx.search(*name) {
                        Some(Binding::Type(typ)) => *typ,
                        Some(Binding::Function { .. }) => {
                            return Err(TypeError::FunctionUsedAsVar)
                        }
                        None => return Err(TypeError::VarUndefined),
                    }
                }
                ParsedAtom::If { if_statement } => {
                    type_if_expression(ctx, if_statement)?
                }
                ParsedAtom::Unit => Type {
                    type_ref: UNIT_TYPE,
                    references: References::new(),
                },
                ParsedAtom::IntegerLiteral(_) => Type {
                    type_ref: INT_TYPE,
                    references: References::new(),
                },
                ParsedAtom::FloatLiteral(_) => Type {
                    type_ref: FLOAT_TYPE,
                    references: References::new(),
                },
            }
        }
        SimpleExprType::BracedExpr(ref mut braced) => {
            type_braced_expr(ctx, braced)?
        }
    };

    for access in expr.accesses.iter_mut() {
        expr_type = match access {
            ParsedExprAccess::Field { name } => { 
                let type_data = ctx.typing_data.lookup_type(expr_type.type_ref);
                match type_data.variant {
                    TypeVariant::Builtin => return Err(TypeError::BuiltinFieldAccess),
                    TypeVariant::Struct(struct_ref) => {
                        let mut field_type = None;

                        for field in ctx.typing_data.lookup_struct(struct_ref).fields.iter() {
                            if field.name == *name {
                                field_type = Some(field.field_type);
                                break
                            }
                        }

                        match field_type {
                            Some(t) => t,
                            None => return Err(TypeError::FieldDoesNotExist)
                        }
                    }
                }
            }
            // not sure about methods yet.
            ParsedExprAccess::Method { name, arguments } => { unimplemented!() }
        }
    }

    expr.ret_type = Some(expr_type);
    Ok(expr_type)
}

fn type_function_call(ctx: &mut Context, name: StrRef, args: &mut [ParsedExpr]) -> Result<Type, TypeError> {
    let name = ctx.search(name);
    match name {
        None => return Err(TypeError::FunctionUndefined),
        Some(Binding::Type(_)) => return Err(TypeError::TypeUsedAsFn),
        Some(Binding::Function { arg_types, return_type }) => {
            if args.len() != arg_types.len() { return Err(TypeError::FnArgLenMismatch) }

            let ret_type = *return_type;

            let arg_types = arg_types.clone(); // TODO unnecessary clone to satisfy lifetimes
            for (arg_expr, arg_type) in args.iter_mut().zip(&*arg_types) {
                let arg_expr_type = type_expr(ctx, arg_expr)?;
                if arg_expr_type != *arg_type { return Err(TypeError::FnArgTypeMismatch) }
            }

            Ok(ret_type)
        }
    }
}
