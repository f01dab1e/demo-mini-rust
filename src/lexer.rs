use std::fmt::Write;

use chumsky::prelude::*;

use crate::ast::BinaryOp;

pub type Span = SimpleSpan<usize>;

static KEYWORDS: phf::Map<&'static str, Token> = phf::phf_map! {
    "fn" => Token::Fn,
    "let" => Token::Let,
    "print" => Token::Print,
    "if" => Token::If,
    "else" => Token::Else,
    "true" => Token::Bool(true),
    "false" => Token::Bool(false),
};

static OPERATORS: phf::Map<&'static str, Token> = phf::phf_map! {
    "+" => Token::Op(BinaryOp::Add),
    "-" => Token::Op(BinaryOp::Sub),
    "*" => Token::Op(BinaryOp::Mul),
    "/" => Token::Op(BinaryOp::Div),
    "==" => Token::Op(BinaryOp::Eq),
    "!=" => Token::Op(BinaryOp::NotEq),
};

pub fn lexer<'input>()
-> impl Parser<'input, &'input str, Vec<(Token<'input>, Span)>, extra::Err<Rich<'input, char, Span>>>
{
    let num = text::int(10)
        .then(just('.').then(text::digits(10)).or_not())
        .slice()
        .from_str()
        .unwrapped()
        .map(Token::Number);

    let string = just('"')
        .ignore_then(none_of('"').repeated())
        .then_ignore(just('"'))
        .map_slice(Token::Str);

    let operator = one_of("+*-/!=").repeated().at_least(1).map_slice(|slice| {
        OPERATORS
            .get(slice)
            .cloned()
            .unwrap_or_else(|| panic!("{slice}"))
    });

    let one_symbol = one_of("()[]{};,").map(|ch| match ch {
        '(' => Token::OpenParen,
        ')' => Token::CloseParen,
        '[' => Token::OpenBracket,
        ']' => Token::CloseBracket,
        '{' => Token::OpenBrace,
        '}' => Token::CloseBrace,
        ';' => Token::Semi,
        ',' => Token::Comma,
        _ => unreachable!(),
    });

    let identifier = text::ident()
        .map(|ident: &str| KEYWORDS.get(ident).cloned().unwrap_or(Token::Ident(ident)));

    let token = num.or(string).or(operator).or(one_symbol).or(identifier);

    let comment = just("//")
        .then(any().and_is(just('\n').not()).repeated())
        .padded();

    token
        .map_with_span(|tok, span| (tok, span))
        .padded_by(comment.repeated())
        .padded()
        .recover_with(skip_then_retry_until(any().ignored(), end()))
        .repeated()
        .collect()
}

#[cfg(all(target_arch = "x86_64", target_pointer_width = "64"))]
static_assert_size!(Token, 24);

#[derive(Clone, Debug, PartialEq)]
pub enum Token<'input> {
    Ident(&'input str),
    Bool(bool),
    Number(f64),
    Str(&'input str),
    Op(BinaryOp),
    OpenParen,
    OpenBrace,
    OpenBracket,
    CloseParen,
    CloseBrace,
    CloseBracket,
    Semi,
    Comma,
    Fn,
    Let,
    Print,
    If,
    Else,
    Equal,
}

impl<'input> Token<'input> {
    pub fn as_binop(&self) -> BinaryOp {
        match self {
            &Self::Op(v) => v,
            _ => unreachable!(),
        }
    }
}

impl<'input> std::fmt::Display for Token<'input> {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Token::Bool(x) => write!(f, "{x}"),
            Token::Number(n) => write!(f, "{n}"),
            Token::Str(s) | Token::Ident(s) => write!(f, "{s}"),
            Token::Fn => write!(f, "fn"),
            Token::Let => write!(f, "let"),
            Token::Print => write!(f, "print"),
            Token::If => write!(f, "if"),
            Token::Else => write!(f, "else"),
            Token::Semi => f.write_char(';'),
            Token::OpenParen => f.write_char('('),
            Token::CloseParen => f.write_char(')'),
            Token::OpenBracket => f.write_char('['),
            Token::CloseBracket => f.write_char(']'),
            Token::OpenBrace => f.write_char('{'),
            Token::CloseBrace => f.write_char('}'),
            Token::Comma => f.write_char(','),
            Token::Op(BinaryOp::Add) => f.write_char('+'),
            Token::Op(BinaryOp::Sub) => f.write_char('-'),
            Token::Op(BinaryOp::Mul) => f.write_char('*'),
            Token::Op(BinaryOp::Div) => f.write_char('/'),
            Token::Op(BinaryOp::Eq) => f.write_str("=="),
            Token::Op(BinaryOp::NotEq) => f.write_str("!="),
            Token::Equal => f.write_char('='),
        }
    }
}

#[cfg(test)]
mod tests {
    use chumsky::Parser;
    use expect_test::{expect, Expect};

    use super::lexer;

    #[track_caller]
    #[allow(clippy::needless_pass_by_value)]
    fn check(input: &str, expect: Expect) {
        let tokens = lexer().parse(input).unwrap();
        let actual: String = tokens
            .into_iter()
            .map(|(token, span)| format!("{token} at {span}\n"))
            .collect();
        expect.assert_eq(&actual);
    }

    #[test]
    fn smoke_test() {
        check(
            "fn main() { println(\"zebra\"); }",
            expect![[r#"
                fn at 0..2
                main at 3..7
                ( at 7..8
                ) at 8..9
                { at 10..11
                println at 12..19
                ( at 19..20
                "zebra" at 20..27
                ) at 27..28
                ; at 28..29
                } at 30..31
            "#]],
        );

        check(
            "// hello\nhello",
            expect![[r#"
                hello at 9..14
            "#]],
        );
    }
}
