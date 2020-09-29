#![feature(box_patterns)]

mod syntax;
mod helper;
mod parser;
mod test;

pub use crate::syntax::Expr::{self, *};
pub use crate::helper::readint;



fn interp_exp(expr: Expr) -> i64 {
    match expr {
        Int ( val ) => val,
        Prim0 ( op ) if op.as_str() == "read" => readint(),
        Prim1 ( op, box e) if op.as_str() == "-" => - interp_exp(e),
        Prim2 ( op, box e1, box e2) if op.as_str() == "+" => interp_exp(e1) + interp_exp(e2),
        _ => panic!("Invalid form!"),
    }
}


fn main() -> std::io::Result<()> {
    use parser::parse;
    use std::io::{self, Write};

    let mut v = String::new();
    loop {
        // input
        print!("User> ");
        io::stdout().flush()?;
        io::stdin().read_line(&mut v)?;
        // parse
        let exp = parse(v.as_str());
        v.clear();
        // interprete
        let res = interp_exp(exp);
        println!("{}", res);
        io::stdout().flush()?;
    }
}
