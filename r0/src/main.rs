#![feature(box_patterns)]

mod semantic;
mod helper;
mod parser;
mod test;

pub use crate::semantic::Expr::{self, Int, Prim};
pub use crate::semantic::Program;
pub use crate::helper::readint;

use parser::parse;



fn interp_exp(expr: &Expr) -> i64 {
    match expr {
        Int ( val ) => *val,
        Prim ( op, box [] ) if op.as_str() == "read" => readint(),
        Prim ( op, box [e]) if op.as_str() == "-" => 0 - interp_exp(e),
        Prim ( op, box [e1, e2]) if op.as_str() == "+" => interp_exp(e1) + interp_exp(e2),
        _ => panic!("Invalid form!"),
    }
}

fn interp_r0(p: &Program) -> i64 {
    match p {
        Program { expr } => interp_exp(expr),
    }
}


fn main() {
    use crate::parser::parse;
    let s = "(+ 1 2)";
    let exp = parse(s);
    println!("{:?}", exp);
}
