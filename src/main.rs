mod lexer;
use lexer::Lexer;

fn main() {
    let input = String::from("1 == 2");
    println!("{:?}", Lexer::new(input).lex());
}
