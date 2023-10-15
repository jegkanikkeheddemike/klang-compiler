use std::{
    env::args,
    fs::{self, read_to_string},
};

mod compiler;
mod lexer;
mod parser;

fn main() {
    let filename = args().nth(1).unwrap();
    let source = read_to_string(&filename).unwrap();
    let no_comments_flag = args().nth(2) == Some("--NO-COMMENTS".into());

    let tokens = lexer::lex(&source);

    let ast = parser::parse_ast(&tokens);
    println!("{:#?}", ast.root);

    let assembly = compiler::compile(&ast, no_comments_flag);
    fs::write(format!("{filename}.out"), &assembly).unwrap();
}
