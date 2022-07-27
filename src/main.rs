mod lexer;
mod ast;
mod parser;
mod interpreter;
mod value;

use std::process::exit;

use colored::*;

use lexer::Token;
use structopt::StructOpt;

#[derive(StructOpt)]
struct Args {
    #[structopt(short="i", long="input", parse(from_os_str))]
    input: std::path::PathBuf,
}

fn main() {
    let args = Args::from_args();
    let input = std::fs::read_to_string(args.input).expect("Unable to read input file");

    let mut lexer = lexer::Lexer::from_str(input.as_str());
    let tokens = lexer.lex();
    if let Err(err) = &tokens {
        error(input.lines(), err.cause.to_owned(), &err.responsible);
    }

    let tokens = tokens.unwrap();
    let parser = parser::Parser::new(&tokens);
    let result = parser.parse();

    if let Err(errs) = result {
        for err in errs {
            error(input.lines(), err.cause, err.responsible);
        }
    } else if let Ok(code) = result {
        let global = interpreter::new();
        code.execute(global).ok();
    }
}

fn error<'a>(mut file: impl Iterator<Item=&'a str>, cause: String, responsible: &Token) {
    let prefix = format!("{}: on line {}: ", "ERROR", responsible.line + 1);
            
    eprintln!("{}{}", prefix.red().bold(), file.nth(responsible.line).unwrap());
    eprintln!("{}{} {}", " ".repeat(prefix.len() + responsible.col), "^".repeat(responsible.span.len()).red().bold(), cause.red().bold());
    println!();

    exit(-1);
} 