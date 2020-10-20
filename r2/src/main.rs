#![feature(box_patterns)]
use std::rc::Rc;

mod syntax;
mod helper;
mod parser;
mod compiler;
mod typesystem;
mod tsort;
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


fn compile(expr: &str, filename: &str) -> std::io::Result<()> {
    use crate::parser::parse;
    use crate::compiler::*;
    let expr = parse(expr);
    // let expr = uniquify(expr);
    let expr = remove_complex_opera(expr);
    let expr = explicate_control(expr);
    let expr = select_instruction(expr);
    let expr = allocate_registers(expr);
    let expr = patch_instructions(expr);
    print_x86(expr, filename);
    Ok(())
}

fn main() -> std::io::Result<()> {
    let e = "(let (x 10)
                (let (y 20)
                (let (c 42)
                    (if (< c y)
                        (+ x 10)
                        (if (eq? x 43)
                            (+ c 20)
                            c)))))";
    compile(e, "r2.asm")
}


     // let e = "(if (if (let (tmp42 (read))
    //                     (eq? tmp42 1))
    //                 (let (tmp43 (read))
    //                     (eq? tmp43 2))
    //                 (let (tmp44 (read))
    //                     (eq? tmp44 3)))
    //             (+ 10 32)
                // (+ 2  40))";
    // let e = "(let (y (read))
    //             (if (if (< y 0)
    //                     (< y -10)
    //                     (< 10 y))
    //                 (+ 10 y)
    //                 (+ 20 y)))";
                    
    // let e = "(let (c (if #t #f #t))
    //            (if c 10 20))";