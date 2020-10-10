#![feature(box_patterns)]
use std::rc::Rc;

mod syntax;
mod helper;
mod parser;
mod compiler;
mod test;

pub use crate::syntax::Expr::{self, *};
pub use crate::syntax::SymTable;


fn interp_exp(expr: Expr, env: Rc<SymTable<String, Expr>>) -> Expr {
    match expr {
        Int(n) => Int(n),
        Bool(b) => Bool(b),
        Var(x) => env.lookup(&x).clone(),
        Prim0(op) if op.as_str() == "read" => Int(helper::readint()),
        Prim1(op, box e) => {
            match interp_exp(e, env) {
                Int(n) if op.as_str() == "-" => Int(-n),
                Bool(b) if op.as_str() == "not" => Bool(!b),
                _ => panic!("mismatch operator!"),
            }
        }
        Prim2(op, box e1, box e2) if helper::is_logical(op.as_str()) => {
            let e1 = interp_exp(e1, Rc::clone(&env));
            match op.as_str() {
                "and" => if e1 == Bool(false) { e1 } else { interp_exp(e2, env) }
                "or" => if e1 == Bool(true) { e1 } else { interp_exp(e2, env) }
                _ => panic!("unknown logical operator"),
            }
        }
        Prim2(op, box e1, box e2) => {
            match (interp_exp(e1, Rc::clone(&env)), interp_exp(e2, env)) {
                (Int(a), Int(b)) => {
                    match op.as_str() {
                        "+" => Int(a + b),
                        "-" => Int(a - b),
                        ">" => Bool(a > b),
                        "<" => Bool(a < b),
                        "<=" => Bool(a <= b),
                        ">=" => Bool(a >= b),
                        "eq?" => Bool(a == b),
                        _ => panic!("unknown operator"),
                    }
                }
                (_, _) => panic!("integer expected"),
            }
        }
        Let(box Var (x), box e1, box e2) => {
            let val = interp_exp(e1, Rc::clone(&env));
            let new_env: SymTable<String, Expr> = SymTable::extend(hashmap!(x => val), &env);
            return interp_exp(e2, Rc::new(new_env));
        } 
        If(box e, box e1, box e2) => if interp_exp(e, Rc::clone(&env)) != Bool(false) { interp_exp(e1, env) } else { interp_exp(e2, env) },
        _ => panic!("bad syntax!"),
    }
}

fn interp_r2(expr: Expr) -> Expr {
    let env = Rc::new(SymTable::new());
    return interp_exp(expr, env);
}


fn main() {
    use parser::parse;
    use compiler::remove_complex_opera;
    let e = "(+ (+ 10 32) (+ 10 3))";
    let exp = parse(e);
    let exp = remove_complex_opera(exp);
    println!("{:?}", exp);
    let r = interp_r2(exp);
    println!("{:?}", r);
}