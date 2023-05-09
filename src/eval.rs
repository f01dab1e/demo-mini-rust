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

pub struct Machine<'me> {
    pub funcs: &'me HashMap<&'me str, Func<'me>>,
    pub stack: Vec<(&'me str, Value<'me>)>,
}

impl<'me> Machine<'me> {
    pub fn eval_expr(&mut self, node: &Spanned<Expr<'me>>) -> Result<Value<'me>, Error> {
        Ok(match &node.0 {
            Expr::Error => unreachable!(),
            Expr::Unit => Value::Unit,
            &Expr::Str(s) => Value::Str(s),
            &Expr::Bool(n) => Value::Bool(n),
            &Expr::Number(n) => Value::Num(n),
            Expr::List(items) => Value::List(
                items
                    .iter()
                    .map(|item| self.eval_expr(item))
                    .collect::<Result<_, _>>()?,
            ),
            Expr::Local(name) => self
                .stack
                .iter()
                .rev()
                .find_map(|(local, value)| (local == name).then(|| value.clone()))
                .or_else(|| self.funcs.contains_key(name).then_some(Value::Func(name)))
                .ok_or_else(|| Error {
                    span: node.1,
                    message: format!("No such variable '{name}' in scope"),
                })?,
            Expr::Let(local, val, body) => {
                let val = self.eval_expr(val)?;
                self.stack.push((local, val));
                let res = self.eval_expr(body)?;
                self.stack.pop();
                res
            }
            Expr::Then(a, b) => {
                self.eval_expr(a)?;
                self.eval_expr(b)?
            }
            Expr::Binary(lhs, op, rhs) => {
                let lhs = self.eval_expr(lhs)?.to_number(lhs.1)?;
                let rhs = self.eval_expr(rhs)?.to_number(rhs.1)?;

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
                let func = self.eval_expr(func_expr)?;
                match func {
                    Value::Func(name) => {
                        let func = &self.funcs[&name];
                        let stack = if func.args.len() == args.len() {
                            func.args
                                .iter()
                                .zip(args.iter())
                                .map(|(name, arg)| self.eval_expr(arg).map(|val| (*name, val)))
                                .collect::<Result<Vec<_>, _>>()?
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

                        self.stack = stack;
                        self.eval_expr(&func.body)?
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
                let test = self.eval_expr(test_expr)?;
                match test {
                    Value::Bool(true) => self.eval_expr(if_true)?,
                    Value::Bool(false) => self.eval_expr(if_false)?,
                    value => {
                        return Err(Error {
                            span: test_expr.1,
                            message: format!("Conditions must be booleans, found '{value:?}'"),
                        });
                    }
                }
            }
            Expr::Print(expr) => {
                let val = self.eval_expr(expr)?;
                println!("{val:?}");
                val
            }
        })
    }
}
