use std::collections::HashMap;

use chumsky::prelude::*;

use crate::ast::{BinaryOp, Expr, ExprKind, Func};
use crate::lexer::{Span, Token};

const BLOCK_END_TOKENS: [Token<'_>; 4] = [
    Token::Semi,
    Token::CloseBrace,
    Token::CloseParen,
    Token::CloseBracket,
];

type ParserInput<'tokens, 'input> =
    chumsky::input::SpannedInput<Token<'input>, Span, &'tokens [(Token<'input>, Span)]>;

#[allow(clippy::too_many_lines)]
fn expr<'tokens, 'input: 'tokens>() -> impl Parser<
    'tokens,
    ParserInput<'tokens, 'input>,
    Expr<'input>,
    extra::Err<Rich<'tokens, Token<'input>, Span>>,
> + Clone {
    recursive(|expr| {
        let inline_expr = recursive(|inline_expr| {
            let val = select! {
                Token::Bool(n) => ExprKind::Bool(n),
                Token::Number(n) => ExprKind::Number(n),
                Token::Str(s) => ExprKind::Str(s),
            }
            .labelled("literal")
            .boxed();

            let ident = select! { Token::Ident(ident) => ident.clone() }.labelled("identifier");

            let items = expr
                .clone()
                .separated_by(just(Token::Comma))
                .allow_trailing()
                .collect::<Vec<_>>();

            let let_expr = just(Token::Let)
                .ignore_then(ident)
                .then_ignore(just(Token::Equal))
                .then(inline_expr)
                .then_ignore(just(Token::Semi))
                .then(expr.clone())
                .map(|((name, val), body)| ExprKind::Let(name, Box::new(val), Box::new(body)))
                .boxed();

            let list = items
                .clone()
                .map(ExprKind::List)
                .delimited_by(just(Token::OpenBracket), just(Token::CloseBracket))
                .boxed();

            let atom = val
                .or(ident.map(ExprKind::Local))
                .or(let_expr)
                .or(list)
                .or(just(Token::Print)
                    .ignore_then(
                        expr.clone()
                            .delimited_by(just(Token::OpenParen), just(Token::CloseParen)),
                    )
                    .map(|expr| ExprKind::Print(Box::new(expr))))
                .map_with_span(mk_expr)
                .or(expr
                    .clone()
                    .delimited_by(just(Token::OpenParen), just(Token::CloseParen)))
                .recover_with(via_parser(nested_delimiters(
                    Token::OpenParen,
                    Token::CloseParen,
                    [
                        (Token::OpenBracket, Token::CloseBracket),
                        (Token::OpenBrace, Token::CloseBrace),
                    ],
                    fallback,
                )))
                .recover_with(via_parser(nested_delimiters(
                    Token::OpenBracket,
                    Token::CloseBracket,
                    [
                        (Token::OpenParen, Token::CloseParen),
                        (Token::OpenBrace, Token::CloseBrace),
                    ],
                    fallback,
                )))
                .boxed();

            let call = atom
                .foldl(
                    items
                        .delimited_by(just(Token::OpenParen), just(Token::CloseParen))
                        .map_with_span(|args, span: Span| (args, span))
                        .repeated(),
                    |callee, (args, args_span)| {
                        let span = callee.span.start..args_span.end;
                        mk_expr(ExprKind::Call(Box::new(callee), args), span.into())
                    },
                )
                .boxed();

            let op = just(Token::Op(BinaryOp::Mul))
                .or(just(Token::Op(BinaryOp::Div)))
                .boxed();

            let product = call
                .clone()
                .foldl(op.then(call).repeated(), |lhs, (operator_token, rhs)| {
                    let span = lhs.span.start..rhs.span.end;
                    mk_expr(
                        ExprKind::Binary(Box::new(lhs), operator_token.as_binop(), Box::new(rhs)),
                        span.into(),
                    )
                })
                .boxed();

            let op = just(Token::Op(BinaryOp::Add)).or(just(Token::Op(BinaryOp::Sub)));

            let sum = product
                .clone()
                .foldl(op.then(product).repeated(), |lhs, (operator_token, rhs)| {
                    let span = lhs.span.start..rhs.span.end;
                    mk_expr(
                        ExprKind::Binary(Box::new(lhs), operator_token.as_binop(), Box::new(rhs)),
                        span.into(),
                    )
                })
                .boxed();

            let op = just(Token::Op(BinaryOp::Eq))
                .or(just(Token::Op(BinaryOp::NotEq)))
                .boxed();

            let compare = sum
                .clone()
                .foldl(op.then(sum).repeated(), |lhs, (operator_token, rhs)| {
                    let span = lhs.span.start..rhs.span.end;
                    mk_expr(
                        ExprKind::Binary(Box::new(lhs), operator_token.as_binop(), Box::new(rhs)),
                        span.into(),
                    )
                })
                .boxed();

            compare.labelled("expression").as_context()
        })
        .boxed();

        let block = expr
            .clone()
            .delimited_by(just(Token::OpenBrace), just(Token::CloseBrace))
            .recover_with(via_parser(nested_delimiters(
                Token::OpenBrace,
                Token::CloseBrace,
                [
                    (Token::OpenParen, Token::CloseParen),
                    (Token::OpenBracket, Token::CloseBracket),
                ],
                fallback,
            )))
            .boxed();

        let if_expr = recursive(|if_| {
            just(Token::If)
                .ignore_then(expr.clone())
                .then(block.clone())
                .then(
                    just(Token::Else)
                        .ignore_then(block.clone().or(if_))
                        .or_not(),
                )
                .map_with_span(|((test, if_true), if_false), span| {
                    mk_expr(
                        ExprKind::If(
                            Box::new(test),
                            Box::new(if_true),
                            Box::new(if_false.unwrap_or_else(|| mk_expr(ExprKind::Unit, span))),
                        ),
                        span,
                    )
                })
        })
        .boxed();

        let block_expr = block.or(if_expr).boxed();
        let block_chain = block_expr
            .clone()
            .foldl(block_expr.clone().repeated(), |a, b| {
                let span = a.span.start..b.span.end;
                mk_expr(ExprKind::Then(Box::new(a), Box::new(b)), span.into())
            })
            .boxed();

        let block_recovery = nested_delimiters(
            Token::OpenBrace,
            Token::CloseBrace,
            [
                (Token::OpenParen, Token::CloseParen),
                (Token::OpenBracket, Token::CloseBracket),
            ],
            |span| (ExprKind::Error, span),
        )
        .boxed();

        block_chain
            .labelled("block")
            .or(inline_expr.clone())
            .recover_with(skip_then_retry_until(
                block_recovery.ignored().or(any().ignored()),
                one_of(BLOCK_END_TOKENS).ignored(),
            ))
            .foldl(
                just(Token::Semi).ignore_then(expr.or_not()).repeated(),
                |head, tail| {
                    let head_start = head.span.start;
                    let tail_end = tail.as_ref().map_or(head.span.end, |b| b.span.end);
                    mk_expr(
                        ExprKind::Then(
                            Box::new(head),
                            Box::new(tail.unwrap_or_else(|| {
                                mk_expr(ExprKind::Unit, (tail_end..tail_end).into())
                            })),
                        ),
                        (head_start..tail_end).into(),
                    )
                },
            )
    })
}

pub fn funcs<'tokens, 'input: 'tokens>() -> impl Parser<
    'tokens,
    ParserInput<'tokens, 'input>,
    HashMap<&'input str, Func<'input>>,
    extra::Err<Rich<'tokens, Token<'input>, Span>>,
> + Clone {
    let ident = select! { Token::Ident(ident) => ident.clone() };

    let args = ident
        .separated_by(just(Token::Comma))
        .allow_trailing()
        .collect()
        .delimited_by(just(Token::OpenParen), just(Token::CloseParen))
        .labelled("function parameters");

    let func = just(Token::Fn)
        .ignore_then(
            ident
                .map_with_span(|name, span| (name, span))
                .labelled("function name"),
        )
        .then(args)
        .map_with_span(|start, span| (start, span))
        .then(
            expr()
                .delimited_by(just(Token::OpenBrace), just(Token::CloseBrace))
                .recover_with(via_parser(nested_delimiters(
                    Token::OpenBrace,
                    Token::CloseBrace,
                    [
                        (Token::OpenParen, Token::CloseParen),
                        (Token::OpenBracket, Token::CloseBracket),
                    ],
                    fallback,
                ))),
        )
        .map(|(((name, args), span), body)| (name, Func { args, span, body }))
        .labelled("function");

    func.repeated()
        .collect::<Vec<_>>()
        .validate(|raw_funcs, _, emitter| {
            let mut funcs = HashMap::new();
            for ((name, name_span), func) in raw_funcs {
                if funcs.insert(name.clone(), func).is_some() {
                    emitter.emit(Rich::custom(
                        name_span,
                        format!("Function '{name}' already exists"),
                    ));
                }
            }
            funcs
        })
}

fn mk_expr(kind: ExprKind<'_>, span: Span) -> Expr<'_> {
    Expr { kind, span }
}

fn fallback<'a>(span: Span) -> Expr<'a> {
    Expr {
        kind: ExprKind::Error,
        span,
    }
}
