#![feature(box_patterns)]

mod syntax;
mod helper;
mod parser;

pub use crate::syntax::Expr::{self, Int, Prim};
pub use crate::syntax::Program;
pub use crate::helper::readint;

use parser::{parse_list, parse};



fn interp_exp(expr: &Expr) -> i64 {
    match expr {
        Int { val } => *val,
        Prim { op: "read", args: box [] } => match readint() {
            Ok(int) => int,
            Err(_) => panic!("expect an integer!"),
        },
        Prim { op: "-", args: box [e] } => 0 - interp_exp(e),
        Prim { op: "+", args: box [e1, e2] } => interp_exp(e1) + interp_exp(e2),
        _ => 42,
    }
}

fn interp_r0(p: &Program) -> i64 {
    match p {
        Program { expr } => interp_exp(expr),
    }
}

#[cfg(test)]
mod tests {
    use crate::*;
    use crate::parser::{List, Atom};
    #[test]
    fn test_r0_1() {
        let p3 = Program {
            expr: Prim { op: "+", 
                        args: Box::new([ Int { val: 10}, Int { val: 32} ])}};
        let r = interp_r0(&p3);
        assert_eq!(r, 42);
    }
    #[test]
    fn test_parse_list() {
        let s = "(1 2 (+ 1 2))";
        let expr = parse_list(s);
        let t = List(vec![Atom("1".to_string()), Atom("2".to_string()), 
                          List(vec![Atom("+".to_string()), Atom("1".to_string()), Atom("2".to_string())])]);
        assert_eq!(expr, t);
    }
    #[test]
    fn test_parse() {
        let s = "(+ 1 2)";
        let expr = Prim {op: "+", args: Box::new([Int{val:1}, Int{val:2}])};
        assert_eq!(parse(s), expr);
        let s = "(- 10)";
        let expr = Prim {op: "-", args: Box::new([Int{val:10}])};
        assert_eq!(parse(s), expr);
        let s = "(read)";
        let expr = Prim {op: "read", args: Box::new([])};
        assert_eq!(parse(s), expr);
    }
    #[test]
    fn test_interp() {
        let s = "(+ 1 2)";
        let expr = parse(s);
        let r = interp_r0(&Program{expr});
        assert_eq!(r, 3);
    }
}

fn main() {
}
