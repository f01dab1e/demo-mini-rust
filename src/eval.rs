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
    fn num(self, span: Span) -> Result<f64, Error> {
        match self {
            Value::Num(x) => Ok(x),
            _ => Err(Error {
                span,
                msg: format!("'{:?}' is not a number", self),
            }),
        }
    }
}

pub struct Error {
    pub span: Span,
    pub msg: String,
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
            .find_map(|(l, v)| (l == name).then(|| v.clone()))
            .or_else(|| funcs.contains_key(name).then_some(Value::Func(name)))
            .ok_or_else(|| Error {
                span: node.1,
                msg: format!("No such variable '{}' in scope", name),
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
        Expr::Binary(a, BinaryOp::Add, b) => {
            Value::Num(expr(a, funcs, stack)?.num(a.1)? + expr(b, funcs, stack)?.num(b.1)?)
        }
        Expr::Binary(a, BinaryOp::Sub, b) => {
            Value::Num(expr(a, funcs, stack)?.num(a.1)? - expr(b, funcs, stack)?.num(b.1)?)
        }
        Expr::Binary(a, BinaryOp::Mul, b) => {
            Value::Num(expr(a, funcs, stack)?.num(a.1)? * expr(b, funcs, stack)?.num(b.1)?)
        }
        Expr::Binary(a, BinaryOp::Div, b) => {
            Value::Num(expr(a, funcs, stack)?.num(a.1)? / expr(b, funcs, stack)?.num(b.1)?)
        }
        Expr::Binary(a, BinaryOp::Eq, b) => {
            Value::Bool(expr(a, funcs, stack)? == expr(b, funcs, stack)?)
        }
        Expr::Binary(a, BinaryOp::NotEq, b) => {
            Value::Bool(expr(a, funcs, stack)? != expr(b, funcs, stack)?)
        }
        Expr::Call(func, args) => {
            let f = expr(func, funcs, stack)?;
            match f {
                Value::Func(name) => {
                    let f = &funcs[&name];
                    let mut stack = if f.args.len() != args.len() {
                        return Err(Error {
                            span: node.1,
                            msg: format!(
                                "'{}' called with wrong number of arguments (expected {}, found {})",
                                name,
                                f.args.len(),
                                args.len()
                            ),
                        });
                    } else {
                        f.args
                            .iter()
                            .zip(args.iter())
                            .map(|(name, arg)| Ok((*name, expr(arg, funcs, stack)?)))
                            .collect::<Result<_, _>>()?
                    };
                    expr(&f.body, funcs, &mut stack)?
                }
                f => {
                    return Err(Error {
                        span: func.1,
                        msg: format!("'{:?}' is not callable", f),
                    });
                }
            }
        }
        Expr::If(cond, a, b) => {
            let test = expr(cond, funcs, stack)?;
            match test {
                Value::Bool(true) => expr(a, funcs, stack)?,
                Value::Bool(false) => expr(b, funcs, stack)?,
                value => {
                    return Err(Error {
                        span: cond.1,
                        msg: format!("Conditions must be booleans, found '{value:?}'"),
                    });
                }
            }
        }
        Expr::Print(a) => {
            let val = expr(a, funcs, stack)?;
            println!("{:?}", val);
            val
        }
    })
}
