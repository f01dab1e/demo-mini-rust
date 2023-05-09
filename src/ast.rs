use crate::lexer::Span;

pub type Spanned<T> = (T, Span);
pub type ExprPtr<'input> = Box<Spanned<Expr<'input>>>;

#[derive(Debug)]
pub enum Expr<'input> {
    Error,
    Literal(Literal<'input>),
    List(Vec<Spanned<Self>>),
    Local(&'input str),
    Let(&'input str, ExprPtr<'input>, ExprPtr<'input>),
    Then(ExprPtr<'input>, ExprPtr<'input>),
    Binary(ExprPtr<'input>, BinaryOp, ExprPtr<'input>),
    Call(ExprPtr<'input>, Vec<Spanned<Expr<'input>>>),
    If(ExprPtr<'input>, ExprPtr<'input>, ExprPtr<'input>),
    Print(ExprPtr<'input>),
}

#[derive(Clone, Debug, PartialEq)]
pub enum Literal<'input> {
    Unit,
    Bool(bool),
    Number(f64),
    Str(&'input str),
}

impl<'input> std::fmt::Display for Literal<'input> {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Self::Unit => write!(f, "null"),
            Self::Bool(x) => write!(f, "{}", x),
            Self::Number(x) => write!(f, "{}", x),
            Self::Str(x) => write!(f, "{}", x),
        }
    }
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
