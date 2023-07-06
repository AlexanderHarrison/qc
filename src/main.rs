use std::io::Read;

pub mod lexer;

mod err;
use err::{err, Unwrappable};
mod parser;
use parser::Parsable;
mod alloc;
mod typing;
// mod mir;
mod compiler;
mod layout;

const USAGE: &'static str = "qc <filename>";

fn main() {
    let f = err!(std::env::args().nth(1), USAGE);
    let mut f = err!(std::fs::File::open(f), USAGE);
    let mut s = String::new();
    err!(f.read_to_string(&mut s), "file error");

    let tokens = err!(lexer::tokenize(&s), "lex error");
    let mut parser = parser::Parser::new(&tokens, &s);

    let mut strings = parser::Strings::new();
    let mut type_names = parser::TypeNames::new(&mut strings);
    let mut parse_data = parser::ParseData { strings, type_names };
    let mut file = parser::ParsedFile::parse(&mut parser, &mut parse_data);

    let type_names = parse_data.type_names;
    let strings = parse_data.strings;

    if let Err(e) = typing::resolve_types(&type_names, &mut file) {
        eprintln!("error: {}", e);
        return;
    }

    let layouts = layout::calculate_layouts(&file.structs, &type_names);

    let mut c_ctx = compiler::CompileCtx::new(&file.structs, layouts);
    let f = &file.fns[0];
    let segment = compiler::compile_segment(&mut c_ctx, f);

    println!("fn {}:", f.name);
    for i in 0..f.input_args.len() {
        let arg = &f.input_args[i];
        let loc = &segment.arg_locations[i];
        let type_name = type_names.get_type_name(arg.arg_type.type_ref);
        match loc {
            Some(compiler::DataLocation::Registers(r)) => println!("\t{}: {} in r{}", arg.name, type_name, compiler::reg_idx(r)),
            Some(compiler::DataLocation::Registers(r)) => println!("\t{}: {} in x{}", arg.name, type_name, compiler::reg_idx(r)),
            None => (),
            _ => todo!(),
        }
    }

    for asm in segment.code.iter() {
        println!("{}", asm);
    }
}
