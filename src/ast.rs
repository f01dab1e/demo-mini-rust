use crate::lexer::Span;

pub type Spanned<T> = (T, Span);
pub type ExprPtr<'input> = Box<Spanned<Expr<'input>>>;

#[derive(Debug)]
pub enum Expr<'input> {
    Error,
    Unit,
    Bool(bool),
    Number(f64),
    Str(&'input str),
    List(Vec<Spanned<Self>>),
    Local(&'input str),
    Let(&'input str, ExprPtr<'input>, ExprPtr<'input>),
    Then(ExprPtr<'input>, ExprPtr<'input>),
    Binary(ExprPtr<'input>, BinaryOp, ExprPtr<'input>),
    Call(ExprPtr<'input>, Vec<Spanned<Expr<'input>>>),
    If(ExprPtr<'input>, ExprPtr<'input>, ExprPtr<'input>),
    Print(ExprPtr<'input>),
}

#[derive(Clone, Debug)]
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
    pub body: Spanned<Expr<'input>>,
}
