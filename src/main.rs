mod lexer;
mod ast;
mod parser;
mod interpreter;

use colored::*;

use structopt::StructOpt;

#[derive(StructOpt)]
struct Args {
    #[structopt(short="i", long="input", parse(from_os_str))]
    input: std::path::PathBuf,
}

fn main() {
    let args = Args::from_args();
    let input = std::fs::read_to_string(args.input).expect("Unable to read input file");

    let mut lexer = lexer::Lexer::new(input.as_str());
    let tokens = lexer.lex();

    let parser = parser::Parser::new(&tokens);
    let result = parser.parse();

    if let Err(errs) = result {
        for err in errs {
            let prefix = format!("{}: on line {}: ", "ERROR", err.responsible.line + 1);

            println!("{}{}", prefix.red().bold(), input.lines().nth(err.responsible.line).unwrap());
            println!("{}{} {}", " ".repeat(prefix.len() + err.responsible.col), "^".repeat(err.responsible.span.len()).red().bold(), err.cause.red().bold());
            println!();
        }
    } else if let Ok(code) = result {
        // dbg!(&code);
        let global = interpreter::new();
        code.execute(global);
    }
}