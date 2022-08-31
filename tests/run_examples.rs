use std::fs;

use ace::prelude::*;

#[test]
fn run_examples() {
    let examples = fs::read_dir("examples").unwrap();

    for example in examples {
        let example = example.unwrap();
        println!("==== {:?} ====", example.file_name());
        let code = fs::read_to_string(example.path()).unwrap();
        let mut lexer = Lexer::from_str(code.as_str());
        let tokens = lexer.lex().unwrap();
        let parser = Parser::new(tokens.as_slice());
        let ast = parser.parse().unwrap();
        let interpreter = new_interpreter();
        ast.execute(interpreter).ok();
    }
}