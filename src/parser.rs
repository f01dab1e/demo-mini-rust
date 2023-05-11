use std::collections::HashMap;

use chumsky::prelude::*;

use crate::ast::{BinaryOp, Expr, ExprKind, Func};
use crate::lexer::{Span, Token};

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
            .labelled("literal");

            let ident = select! { Token::Ident(ident) => ident.clone() }.labelled("identifier");

            let items = expr
                .clone()
                .separated_by(just(Token::Char(',')))
                .allow_trailing()
                .collect::<Vec<_>>();

            let let_expr = just(Token::Let)
                .ignore_then(ident)
                .then_ignore(just(Token::Op("=")))
                .then(inline_expr)
                .then_ignore(just(Token::Char(';')))
                .then(expr.clone())
                .map(|((name, val), body)| ExprKind::Let(name, Box::new(val), Box::new(body)));

            let list = items
                .clone()
                .map(ExprKind::List)
                .delimited_by(just(Token::Char('[')), just(Token::Char(']')));

            let atom = val
                .or(ident.map(ExprKind::Local))
                .or(let_expr)
                .or(list)
                .or(just(Token::Print)
                    .ignore_then(
                        expr.clone()
                            .delimited_by(just(Token::Char('(')), just(Token::Char(')'))),
                    )
                    .map(|expr| ExprKind::Print(Box::new(expr))))
                .map_with_span(mk_expr)
                .or(expr
                    .clone()
                    .delimited_by(just(Token::Char('(')), just(Token::Char(')'))))
                .recover_with(via_parser(nested_delimiters(
                    Token::Char('('),
                    Token::Char(')'),
                    [
                        (Token::Char('['), Token::Char(']')),
                        (Token::Char('{'), Token::Char('}')),
                    ],
                    fallback,
                )))
                .recover_with(via_parser(nested_delimiters(
                    Token::Char('['),
                    Token::Char(']'),
                    [
                        (Token::Char('('), Token::Char(')')),
                        (Token::Char('{'), Token::Char('}')),
                    ],
                    fallback,
                )))
                .boxed();

            let call = atom.foldl(
                items
                    .delimited_by(just(Token::Char('(')), just(Token::Char(')')))
                    .map_with_span(|args, span: Span| (args, span))
                    .repeated(),
                |callee, args| {
                    let span = callee.span.start..args.1.end;
                    mk_expr(ExprKind::Call(Box::new(callee), args.0), span.into())
                },
            );

            let op = just(Token::Op("*"))
                .to(BinaryOp::Mul)
                .or(just(Token::Op("/")).to(BinaryOp::Div));

            let product = call
                .clone()
                .foldl(op.then(call).repeated(), |lhs, (op, rhs)| {
                    let span = lhs.span.start..rhs.span.end;
                    mk_expr(
                        ExprKind::Binary(Box::new(lhs), op, Box::new(rhs)),
                        span.into(),
                    )
                });

            let op = just(Token::Op("+"))
                .to(BinaryOp::Add)
                .or(just(Token::Op("-")).to(BinaryOp::Sub));

            let sum = product
                .clone()
                .foldl(op.then(product).repeated(), |lhs, (op, rhs)| {
                    let span = lhs.span.start..rhs.span.end;
                    mk_expr(
                        ExprKind::Binary(Box::new(lhs), op, Box::new(rhs)),
                        span.into(),
                    )
                });

            let op = just(Token::Op("=="))
                .to(BinaryOp::Eq)
                .or(just(Token::Op("!=")).to(BinaryOp::NotEq));

            let compare = sum
                .clone()
                .foldl(op.then(sum).repeated(), |lhs, (op, rhs)| {
                    let span = lhs.span.start..rhs.span.end;
                    mk_expr(
                        ExprKind::Binary(Box::new(lhs), op, Box::new(rhs)),
                        span.into(),
                    )
                });

            compare.labelled("expression").as_context()
        });

        let block = expr
            .clone()
            .delimited_by(just(Token::Char('{')), just(Token::Char('}')))
            .recover_with(via_parser(nested_delimiters(
                Token::Char('{'),
                Token::Char('}'),
                [
                    (Token::Char('('), Token::Char(')')),
                    (Token::Char('['), Token::Char(']')),
                ],
                fallback,
            )));

        let if_expr = recursive(|if_| {
            just(Token::If)
                .ignore_then(expr.clone())
                .then(block.clone())
                .then(
                    just(Token::Else)
                        .ignore_then(block.clone().or(if_))
                        .or_not(),
                )
                .map_with_span(|((test, if_true), if_false), span: Span| {
                    mk_expr(
                        ExprKind::If(
                            Box::new(test),
                            Box::new(if_true),
                            Box::new(if_false.unwrap_or_else(|| mk_expr(ExprKind::Unit, span))),
                        ),
                        span,
                    )
                })
        });

        let block_expr = block.or(if_expr);
        let block_chain = block_expr
            .clone()
            .foldl(block_expr.clone().repeated(), |a, b| {
                let span = a.span.start..b.span.end;
                mk_expr(ExprKind::Then(Box::new(a), Box::new(b)), span.into())
            });

        let block_recovery = nested_delimiters(
            Token::Char('{'),
            Token::Char('}'),
            [
                (Token::Char('('), Token::Char(')')),
                (Token::Char('['), Token::Char(']')),
            ],
            |span| (ExprKind::Error, span),
        );

        block_chain
            .labelled("block")
            .or(inline_expr.clone())
            .recover_with(skip_then_retry_until(
                block_recovery.ignored().or(any().ignored()),
                one_of([
                    Token::Char(';'),
                    Token::Char('}'),
                    Token::Char(')'),
                    Token::Char(']'),
                ])
                .ignored(),
            ))
            .foldl(
                just(Token::Char(';')).ignore_then(expr.or_not()).repeated(),
                |a, b| {
                    let a_start = a.span.start;
                    let b_end = b.as_ref().map_or(a.span.end, |b| b.span.end);
                    mk_expr(
                        ExprKind::Then(
                            Box::new(a),
                            Box::new(
                                b.unwrap_or_else(|| mk_expr(ExprKind::Unit, (b_end..b_end).into())),
                            ),
                        ),
                        (a_start..b_end).into(),
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
        .separated_by(just(Token::Char(',')))
        .allow_trailing()
        .collect()
        .delimited_by(just(Token::Char('(')), just(Token::Char(')')))
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
                .delimited_by(just(Token::Char('{')), just(Token::Char('}')))
                .recover_with(via_parser(nested_delimiters(
                    Token::Char('{'),
                    Token::Char('}'),
                    [
                        (Token::Char('('), Token::Char(')')),
                        (Token::Char('['), Token::Char(']')),
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

fn mk_expr(kind: ExprKind, span: Span) -> Expr {
    Expr { kind, span }
}

fn fallback<'a>(span: Span) -> Expr<'a> {
    Expr {
        kind: ExprKind::Error,
        span,
    }
}
