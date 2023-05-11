use std::collections::HashMap;

use crate::ast::{BinaryOp, Expr, ExprKind, Func};
use crate::lexer::Span;

#[cfg(all(target_arch = "x86_64", target_pointer_width = "64"))]
static_assert_size!(Value, 32);

#[derive(Clone, Debug, PartialEq)]
pub enum Value<'input> {
    Unit,
    Bool(bool),
    Number(f64),
    String(&'input str),
    List(Vec<Self>),
    Function(&'input str),
}

impl<'input> Value<'input> {
    fn to_number(&self, span: Span) -> Result<f64, Error> {
        match self {
            &Value::Number(x) => Ok(x),
            _ => Err(Error {
                span,
                message: format!("'{self:?}' is not a number"),
            }),
        }
    }
}

#[derive(Debug)]
pub struct Error {
    pub span: Span,
    pub message: String,
}

pub struct Machine<'me> {
    pub funcs: &'me HashMap<&'me str, Func<'me>>,
    pub stack: Vec<(&'me str, Value<'me>)>,
}

impl<'me> Machine<'me> {
    #[allow(clippy::too_many_lines)]
    pub fn eval_expr(&mut self, node: &'me Expr) -> Result<Value<'me>, Error> {
        Ok(match &node.kind {
            ExprKind::Error => unreachable!(),
            ExprKind::Unit => Value::Unit,
            &ExprKind::Str(s) => Value::String(s),
            &ExprKind::Bool(n) => Value::Bool(n),
            &ExprKind::Number(n) => Value::Number(n),
            ExprKind::List(items) => Value::List(
                items
                    .iter()
                    .map(|item| self.eval_expr(item))
                    .collect::<Result<_, _>>()?,
            ),
            ExprKind::Local(name) => self
                .stack
                .iter()
                .rev()
                .find_map(|(local, value)| (local == name).then(|| value.clone()))
                .or_else(|| {
                    self.funcs
                        .contains_key(name)
                        .then_some(Value::Function(name))
                })
                .ok_or_else(|| Error {
                    span: node.span,
                    message: format!("No such variable '{name}' in scope"),
                })?,
            ExprKind::Let(local, val, body) => {
                let val = self.eval_expr(val)?;
                self.stack.push((local, val));
                let res = self.eval_expr(body)?;
                self.stack.pop();
                res
            }
            ExprKind::Then(a, b) => {
                self.eval_expr(a)?;
                self.eval_expr(b)?
            }
            ExprKind::Binary(lhs, op, rhs) => {
                let lhs = self.eval_expr(lhs)?.to_number(lhs.span)?;
                let rhs = self.eval_expr(rhs)?.to_number(rhs.span)?;

                let binary = match op {
                    BinaryOp::Add => std::ops::Add::add,
                    BinaryOp::Sub => std::ops::Sub::sub,
                    BinaryOp::Mul => std::ops::Mul::mul,
                    BinaryOp::Div => std::ops::Div::div,
                    BinaryOp::Eq => return Ok(Value::Bool((lhs - rhs).abs() < f64::EPSILON)),
                    BinaryOp::NotEq => return Ok(Value::Bool((lhs - rhs).abs() > f64::EPSILON)),
                };

                Value::Number(binary(lhs, rhs))
            }
            ExprKind::Call(func_expr, args) => {
                let func = self.eval_expr(func_expr)?;
                match func {
                    Value::Function(name) => {
                        let func = &self.funcs[&name];
                        let stack = if func.args.len() == args.len() {
                            func.args
                                .iter()
                                .zip(args.iter())
                                .map(|(name, arg)| self.eval_expr(arg).map(|val| (*name, val)))
                                .collect::<Result<Vec<_>, _>>()?
                        } else {
                            return Err(Error {
                                span: node.span,
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
                            span: func_expr.span,
                            message: format!("'{not_callable:?}' is not callable"),
                        });
                    }
                }
            }
            ExprKind::If(test_expr, if_true, if_false) => {
                let test = self.eval_expr(test_expr)?;
                match test {
                    Value::Bool(true) => self.eval_expr(if_true)?,
                    Value::Bool(false) => self.eval_expr(if_false)?,
                    value => {
                        return Err(Error {
                            span: test_expr.span,
                            message: format!("Conditions must be booleans, found '{value:?}'"),
                        });
                    }
                }
            }
            ExprKind::Print(expr) => {
                let val = self.eval_expr(expr)?;
                println!("{val:?}");
                val
            }
        })
    }
}

#[cfg(test)]
mod tests {
    use chumsky::prelude::Input;
    use chumsky::Parser;

    use super::{Machine, Value};

    #[track_caller]
    #[allow(clippy::needless_pass_by_value)]
    fn eval_expr(input: &str, expect: Value) {
        let input = format!("fn main() {{ {input} }}");
        let tokens = crate::lexer::lexer().parse(&input).unwrap();
        let eoi = input.len()..input.len();
        let funcs = crate::parser::funcs()
            .parse(tokens.as_slice().spanned(eoi.into()))
            .unwrap();

        let mut machine = Machine {
            funcs: &funcs,
            stack: Vec::new(),
        };
        let func = &funcs["main"];
        assert_eq!(machine.eval_expr(&func.body).unwrap(), expect);
    }

    #[test]
    fn smoke_test() {
        eval_expr("true", Value::Bool(true));
        eval_expr("false", Value::Bool(false));

        eval_expr("42 == 42", Value::Bool(true));
        eval_expr("42 != 42", Value::Bool(false));

        eval_expr("if true { 42 } else { 40 }", Value::Number(42.));
    }
}
