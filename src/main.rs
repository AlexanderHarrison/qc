use std::io::Read;

pub mod lexer;

mod err;
use err::{err, Unwrappable};
mod parser;
use parser::Parsable;
mod alloc;
mod compiler;
mod typing;

const USAGE: &'static str = "qc <filename>";

fn main() {
    let f = err!(std::env::args().nth(1), USAGE);
    let mut f = err!(std::fs::File::open(f), USAGE);

    let mut s = String::new();
    err!(f.read_to_string(&mut s), "file error");
    let tokens = err!(lexer::tokenize(&s), "lex error");
    let mut parser = parser::Parser::new(&tokens, &s);
    let mut data = parser::ParseData::new();
    let file = parser::ParsedFile::parse(&mut parser, &mut data);
    println!("{}", file.fns.len());
}
