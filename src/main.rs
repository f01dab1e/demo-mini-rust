use anyhow::{bail, Context};
use ariadne::{sources, Color, Label, Report, ReportKind};
use chumsky::prelude::Input;
use chumsky::Parser;

mod ast;
mod lexer;
mod parser;

fn main() -> anyhow::Result<()> {
    let mut args = std::env::args().skip(1);

    match args.next() {
        Some(path) if args.len() == 0 => {
            let input =
                std::fs::read_to_string(&path).with_context(|| format!("reading `{path}`"))?;
            let (tokens, errors) = lexer::lexer().parse(&input).into_output_errors();

            let parse_errors = match &tokens {
                Some(tokens) => {
                    let eoi = input.len()..input.len();
                    let (funcs, parse_errors) = parser::funcs()
                        .parse(tokens.as_slice().spanned(eoi.into()))
                        .into_output_errors();

                    dbg!(&funcs);

                    parse_errors
                }
                None => Vec::new(),
            };

            let errors = errors
                .into_iter()
                .map(|e| e.map_token(|c| c.to_string()))
                .chain(
                    parse_errors
                        .into_iter()
                        .map(|e| e.map_token(|tok| tok.to_string())),
                );

            for error in errors {
                Report::build(ReportKind::Error, path.clone(), error.span().start)
                    .with_message(error.to_string())
                    .with_label(
                        Label::new((path.clone(), error.span().into_range()))
                            .with_message(error.reason().to_string())
                            .with_color(Color::Red),
                    )
                    .with_labels(error.contexts().map(|(label, span)| {
                        Label::new((path.clone(), span.into_range()))
                            .with_message(format!("while parsing this {}", label))
                            .with_color(Color::Yellow)
                    }))
                    .finish()
                    .print(sources([(path.clone(), input.to_string())]))?;
            }
        }
        Some(_) => bail!("you must specify exactly one input file"),
        None => bail!("you must specify an input file"),
    }

    Ok(())
}
