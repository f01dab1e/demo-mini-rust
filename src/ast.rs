use crate::lexer::Span;

pub type ExprPtr<'input> = Box<Expr<'input>>;

#[cfg(all(target_arch = "x86_64", target_pointer_width = "64"))]
static_assert_size!(Expr, 56);

#[derive(Debug)]
pub struct Expr<'input> {
    pub kind: ExprKind<'input>,
    pub span: Span,
}

#[derive(Debug)]
pub enum ExprKind<'input> {
    Error,
    Unit,
    Bool(bool),
    Number(f64),
    Str(&'input str),
    List(Vec<Expr<'input>>),
    Local(&'input str),
    Let(&'input str, ExprPtr<'input>, ExprPtr<'input>),
    Then(ExprPtr<'input>, ExprPtr<'input>),
    Binary(ExprPtr<'input>, BinaryOp, ExprPtr<'input>),
    Call(ExprPtr<'input>, Vec<Expr<'input>>),
    If(ExprPtr<'input>, ExprPtr<'input>, ExprPtr<'input>),
    Print(ExprPtr<'input>),
}

#[derive(Clone, Debug, PartialEq)]
pub enum BinaryOp {
    Add,
    Sub,
    Mul,
    Div,
    Eq,
    NotEq,
}

#[derive(Debug)]
pub struct Func<'input> {
    pub args: Vec<&'input str>,
    pub span: Span,
    pub body: Expr<'input>,
}
