# demo-mini-rust 

## Abstract
This is a dynamically-typed programming language called mini-rust, inspired by Rust and equipped with a parser and interpreter.

During the demo project's development, I worked extensively with parsing techniques and parser combinators. In addition, I efficiently used powerful crates such as Chumsky, phf (perfect hash function), ariadne, and expect-test.

I utilized the Chumsky library to construct powerful and flexible parsers by combining smaller parsing functions, resulting in a more efficient and streamlined parsing process. Also, I employed the phf (perfect hash function) library to generate compact hash-based data structures, significantly improving lookup performance and ensuring seamless data retrieval, even when handling large datasets.

The solution offers efficient code processing and reliable error handling, providing a solid foundation for expressive and flexible programming.

### Try it out!

1. Install [Rust](https://rustup.rs/)
2. Run the app
```bash
cargo run --release -- demo
```
### Example

[![asciicast](https://asciinema.org/a/V9uUpaiZ5mt8unPMgw5Nwunon.svg)](https://asciinema.org/a/V9uUpaiZ5mt8unPMgw5Nwunon)
