use std::collections::HashMap;

use crate::ast::{BinaryOp, Expr, Func, Spanned};
use crate::lexer::Span;

#[derive(Clone, Debug, PartialEq)]
pub enum Value<'input> {
    Unit,
    Bool(bool),
    Num(f64),
    Str(&'input str),
    List(Vec<Self>),
    Func(&'input str),
}

impl<'input> Value<'input> {
    fn to_number(&self, span: Span) -> Result<f64, Error> {
        match self {
            &Value::Num(x) => Ok(x),
            _ => Err(Error {
                span,
                message: format!("'{self:?}' is not a number"),
            }),
        }
    }
}

pub struct Error {
    pub span: Span,
    pub message: String,
}

pub fn expr<'input>(
    node: &Spanned<Expr<'input>>,
    funcs: &HashMap<&'input str, Func<'input>>,
    stack: &mut Vec<(&'input str, Value<'input>)>,
) -> Result<Value<'input>, Error> {
    Ok(match &node.0 {
        Expr::Error => unreachable!(),
        Expr::Unit => Value::Unit,
        &Expr::Str(s) => Value::Str(s),
        &Expr::Bool(n) => Value::Bool(n),
        &Expr::Number(n) => Value::Num(n),
        Expr::List(items) => Value::List(
            items
                .iter()
                .map(|item| expr(item, funcs, stack))
                .collect::<Result<_, _>>()?,
        ),
        Expr::Local(name) => stack
            .iter()
            .rev()
            .find_map(|(local, value)| (local == name).then(|| value.clone()))
            .or_else(|| funcs.contains_key(name).then_some(Value::Func(name)))
            .ok_or_else(|| Error {
                span: node.1,
                message: format!("No such variable '{name}' in scope"),
            })?,
        Expr::Let(local, val, body) => {
            let val = expr(val, funcs, stack)?;
            stack.push((local, val));
            let res = expr(body, funcs, stack)?;
            stack.pop();
            res
        }
        Expr::Then(a, b) => {
            expr(a, funcs, stack)?;
            expr(b, funcs, stack)?
        }
        Expr::Binary(lhs, op, rhs) => {
            let lhs = expr(lhs, funcs, stack)?.to_number(lhs.1)?;
            let rhs = expr(rhs, funcs, stack)?.to_number(rhs.1)?;

            let binary = match op {
                BinaryOp::Add => std::ops::Add::add,
                BinaryOp::Sub => std::ops::Sub::sub,
                BinaryOp::Mul => std::ops::Mul::mul,
                BinaryOp::Div => std::ops::Div::div,
                BinaryOp::Eq => return Ok(Value::Bool((lhs - rhs).abs() < f64::EPSILON)),
                BinaryOp::NotEq => return Ok(Value::Bool((lhs - rhs).abs() > f64::EPSILON)),
            };

            Value::Num(binary(lhs, rhs))
        }
        Expr::Call(func_expr, args) => {
            let func = expr(func_expr, funcs, stack)?;
            match func {
                Value::Func(name) => {
                    let func = &funcs[&name];
                    let mut stack = if func.args.len() == args.len() {
                        func.args
                            .iter()
                            .zip(args.iter())
                            .map(|(name, arg)| Ok((*name, expr(arg, funcs, stack)?)))
                            .collect::<Result<_, _>>()?
                    } else {
                        return Err(Error {
                            span: node.1,
                            message: format!(
                                "'{name}' called with wrong number of arguments (expected {}, found {})",
                                func.args.len(),
                                args.len()
                            ),
                        });
                    };
                    expr(&func.body, funcs, &mut stack)?
                }
                not_callable => {
                    return Err(Error {
                        span: func_expr.1,
                        message: format!("'{not_callable:?}' is not callable"),
                    });
                }
            }
        }
        Expr::If(test_expr, if_true, if_false) => {
            let test = expr(test_expr, funcs, stack)?;
            match test {
                Value::Bool(true) => expr(if_true, funcs, stack)?,
                Value::Bool(false) => expr(if_false, funcs, stack)?,
                value => {
                    return Err(Error {
                        span: test_expr.1,
                        message: format!("Conditions must be booleans, found '{value:?}'"),
                    });
                }
            }
        }
        Expr::Print(a) => {
            let val = expr(a, funcs, stack)?;
            println!("{val:?}");
            val
        }
    })
}
