use std::collections::HashMap;

use chumsky::prelude::*;

use crate::ast::{BinaryOp, Expr, Func, Literal, Spanned};
use crate::lexer::{Span, Token};

type ParserInput<'tokens, 'input> =
    chumsky::input::SpannedInput<Token<'input>, Span, &'tokens [(Token<'input>, Span)]>;

fn expr<'tokens, 'input: 'tokens>() -> impl Parser<
    'tokens,
    ParserInput<'tokens, 'input>,
    Spanned<Expr<'input>>,
    extra::Err<Rich<'tokens, Token<'input>, Span>>,
> + Clone {
    recursive(|expr| {
        let inline_expr = recursive(|inline_expr| {
            let val = select! {
                Token::Null => Expr::Literal(Literal::Unit),
                Token::Bool(n) => Expr::Literal(Literal::Bool(n)),
                Token::Number(n) => Expr::Literal(Literal::Number(n)),
                Token::Str(s) => Expr::Literal(Literal::Str(s)),
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
                .map(|((name, val), body)| Expr::Let(name, Box::new(val), Box::new(body)));

            let list = items
                .clone()
                .map(Expr::List)
                .delimited_by(just(Token::Char('[')), just(Token::Char(']')));

            let atom = val
                .or(ident.map(Expr::Local))
                .or(let_expr)
                .or(list)
                .or(just(Token::Print)
                    .ignore_then(
                        expr.clone()
                            .delimited_by(just(Token::Char('(')), just(Token::Char(')'))),
                    )
                    .map(|expr| Expr::Print(Box::new(expr))))
                .map_with_span(|expr, span| (expr, span))
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
                    |span| (Expr::Error, span),
                )))
                .recover_with(via_parser(nested_delimiters(
                    Token::Char('['),
                    Token::Char(']'),
                    [
                        (Token::Char('('), Token::Char(')')),
                        (Token::Char('{'), Token::Char('}')),
                    ],
                    |span| (Expr::Error, span),
                )))
                .boxed();

            let call = atom.foldl(
                items
                    .delimited_by(just(Token::Char('(')), just(Token::Char(')')))
                    .map_with_span(|args, span: Span| (args, span))
                    .repeated(),
                |f, args| {
                    let span = f.1.start..args.1.end;
                    (Expr::Call(Box::new(f), args.0), span.into())
                },
            );

            let op = just(Token::Op("*"))
                .to(BinaryOp::Mul)
                .or(just(Token::Op("/")).to(BinaryOp::Div));

            let product = call.clone().foldl(op.then(call).repeated(), |a, (op, b)| {
                let span = a.1.start..b.1.end;
                (Expr::Binary(Box::new(a), op, Box::new(b)), span.into())
            });

            let op = just(Token::Op("+"))
                .to(BinaryOp::Add)
                .or(just(Token::Op("-")).to(BinaryOp::Sub));

            let sum = product
                .clone()
                .foldl(op.then(product).repeated(), |a, (op, b)| {
                    let span = a.1.start..b.1.end;
                    (Expr::Binary(Box::new(a), op, Box::new(b)), span.into())
                });

            let op = just(Token::Op("=="))
                .to(BinaryOp::Eq)
                .or(just(Token::Op("!=")).to(BinaryOp::NotEq));
            let compare = sum.clone().foldl(op.then(sum).repeated(), |a, (op, b)| {
                let span = a.1.start..b.1.end;
                (Expr::Binary(Box::new(a), op, Box::new(b)), span.into())
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
                |span| (Expr::Error, span),
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
                .map_with_span(|((cond, a), b), span: Span| {
                    (
                        Expr::If(
                            Box::new(cond),
                            Box::new(a),
                            Box::new(b.unwrap_or_else(|| (Expr::Literal(Literal::Unit), span))),
                        ),
                        span,
                    )
                })
        });

        let block_expr = block.or(if_expr);
        let block_chain = block_expr
            .clone()
            .foldl(block_expr.clone().repeated(), |a, b| {
                let span = a.1.start..b.1.end;
                (Expr::Then(Box::new(a), Box::new(b)), span.into())
            });

        let block_recovery = nested_delimiters(
            Token::Char('{'),
            Token::Char('}'),
            [
                (Token::Char('('), Token::Char(')')),
                (Token::Char('['), Token::Char(']')),
            ],
            |span| (Expr::Error, span),
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
                    let a_start = a.1.start;
                    let b_end = b.as_ref().map(|b| b.1.end).unwrap_or(a.1.end);
                    (
                        Expr::Then(
                            Box::new(a),
                            Box::new(b.unwrap_or_else(|| {
                                (Expr::Literal(Literal::Unit), (b_end..b_end).into())
                            })),
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
        .labelled("function args");

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
                    |span| (Expr::Error, span),
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
                        format!("Function '{}' already exists", name),
                    ));
                }
            }
            funcs
        })
}
