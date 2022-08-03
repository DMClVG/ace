mod ast;
mod interpreter;
mod lexer;
mod parser;
mod value;

use std::{borrow::Cow, io::Write, process::exit};

use colored::*;

use lexer::Token;
use rustyline::{
    highlight::{Highlighter, MatchingBracketHighlighter},
    hint::HistoryHinter,
    Config,
};
use rustyline_derive::*;
use structopt::StructOpt;

#[derive(StructOpt)]
struct Args {
    #[structopt(short = "i", long = "input", parse(from_os_str))]
    input_file: Option<std::path::PathBuf>,
    #[structopt(long = "ast")]
    print_ast: bool,
}

#[derive(Helper, Completer, Hinter, Validator)]
struct PromptHelper {
    highlighter: MatchingBracketHighlighter,
    colored_prompt: String,
}

impl Highlighter for PromptHelper {
    // fn highlight_prompt<'b, 's: 'b, 'p: 'b>(
    //     &'s self,
    //     prompt: &'p str,
    //     default: bool,
    // ) -> Cow<'b, str> {
    //     if default {
    //         Cow::Borrowed(&self.colored_prompt)
    //     } else {
    //         Cow::Borrowed(prompt)
    //     }
    // }

    fn highlight_hint<'h>(&self, hint: &'h str) -> Cow<'h, str> {
        Cow::Owned("\x1b[1m".to_owned() + hint + "\x1b[m")
    }

    fn highlight<'l>(&self, line: &'l str, pos: usize) -> Cow<'l, str> {
        self.highlighter.highlight(line, pos)
    }

    fn highlight_char(&self, line: &str, pos: usize) -> bool {
        self.highlighter.highlight_char(line, pos)
    }
}

fn interactable(args: &Args) {
    let interpreter = interpreter::new();
    let indicator = ">>> ";
    let mut rl = rustyline::Editor::<PromptHelper>::with_config(
        Config::builder()
            .edit_mode(rustyline::EditMode::Emacs)
            .auto_add_history(true)
            .build(),
    )
    .unwrap();

    let helper = PromptHelper {
        highlighter: MatchingBracketHighlighter::new(),
        colored_prompt: "".to_owned(),
    };

    rl.set_helper(Some(helper));
    'run: loop {
        std::io::stdout().flush().unwrap();
        print!("{} ", indicator);
        std::io::stdout().flush().unwrap();

        let read;
        match rl.readline(indicator) {
            Ok(res) => read = res,
            Err(_) => break 'run,
        }

        if read.trim_end() == "exit" {
            break;
        } else {
            let mut lexer = lexer::Lexer::from_str(read.as_str());
            let tokens = lexer.lex();
            if let Err(err) = &tokens {
                error(
                    Some(read.as_str()).into_iter(),
                    err.cause.to_owned(),
                    &err.responsible,
                    false,
                );
                continue 'run;
            }
            let tokens = tokens.unwrap();
            let ast = parser::Parser::new(&tokens).parse();
            if let Err(errs) = &ast {
                for err in errs {
                    error(
                        Some(read.as_str()).into_iter(),
                        err.cause.to_owned(),
                        err.responsible,
                        false,
                    );
                    continue 'run;
                }
            }
            let ast = ast.unwrap();

            if args.print_ast {
                println!("{:#?}", ast);
            } else {
                match ast.execute(interpreter.clone()) {
                    Err(v) if !v.is_nil() => println!("{}", v),
                    Ok(v) if !v.is_nil() => println!("{}", v),
                    _ => {}
                }
            }
        }
    }
    std::io::stdout().flush().unwrap();
}

fn main() {
    let args = Args::from_args();

    if let Some(input_file) = args.input_file {
        let input = std::fs::read_to_string(input_file).expect("Unable to read input file");

        let mut lexer = lexer::Lexer::from_str(input.as_str());
        let tokens = lexer.lex();
        if let Err(err) = &tokens {
            error(input.lines(), err.cause.to_owned(), &err.responsible, true);
            exit(-1);
        }

        let tokens = tokens.unwrap();
        let parser = parser::Parser::new(&tokens);
        let result = parser.parse();

        if let Err(errs) = result {
            for err in errs {
                error(input.lines(), err.cause, err.responsible, true);
                exit(-1);
            }
        } else if let Ok(code) = result {
            if args.print_ast {
                println!("{:#?}", code);
            } else {
                let global = interpreter::new();
                code.execute(global).ok();
            }
        }
    } else {
        interactable(&args);
    }
}

fn error<'a>(
    mut file: impl Iterator<Item = &'a str>,
    cause: String,
    responsible: &Token,
    print_line_number: bool,
) {
    let prefix = format!(
        "{}: {}",
        "ERROR",
        if print_line_number {
            format!("on line {}: ", responsible.line + 1)
        } else {
            "".to_owned()
        }
    );

    eprintln!(
        "{}{}",
        prefix.red().bold(),
        file.nth(responsible.line).unwrap().trim_end()
    );
    eprintln!(
        "{}{} {}",
        " ".repeat(prefix.len() + responsible.col),
        "^".repeat(responsible.span.len()).red().bold(),
        cause.red().bold()
    );

    // exit(-1);
}
