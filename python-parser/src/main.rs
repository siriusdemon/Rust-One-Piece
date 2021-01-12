// let's start from very simple syntax
// x
// x = 10

// syntax
#[derive(Debug)]
enum Expr {
    Var(String),
    Assign(Box<Expr>, Box<Expr>),
}


// marker
const WHITESPACE: char = ' ';
const COLON: char = ':';
const SEMICOLON: char = ';';
const COMMA: char = ',';

