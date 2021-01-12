#![feature(box_patterns)]
use std::rc::Rc;
use std::cell::RefCell;
use std::mem;

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
        Void => Void,
        Int(n) => Int(n),
        Bool(b) => Bool(b),
        Var(x) => match env.lookup(&x) {
            Reference(r) => Reference(Rc::clone(r)),
            Var(y) => interp_exp(Var(y.to_string()), env),
            e => e.clone(),
        }
        // Vector(v) => Vector(v), // should not occur
        Reference(x) => Reference(x),
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
        Prim2(op, box e, box index) if op.as_str() == "vector-ref" => {
            let index = interp_exp(index, Rc::clone(&env));
            let vector = interp_exp(e, env);
            match (index, vector) {
                (Int(n), Reference(v)) => {
                    let v = v.borrow(); // Vector
                    match &*v {
                        Vector(iv) => iv[n as usize].clone(),
                        _ => panic!("unknown type"),
                    }
                }
                _ => panic!("unknown type"),
            }
        }
        Prim3(op, box e1, box index, box e2) if op.as_str() == "vector-set!" => {
            let index = interp_exp(index, Rc::clone(&env));
            let mut vector = interp_exp(e1, Rc::clone(&env));
            let e2 = interp_exp(e2, env);
            match (index, vector) {
                (Int(n), Reference(v)) => {
                    let mut v = v.borrow_mut();
                    match &mut *v {
                        Vector(ref mut iv) => {
                            iv[n as usize] = e2;
                            return Void;
                        }
                        _ => panic!("unknown type"),
                    }
                }
                (_, _) => {
                    panic!("unknown type");
                }
            }
        }
        PrimN(op, exprs) if op.as_str() == "vector" => {
            let exprs: Vec<Expr> = exprs.into_iter().map(|e| interp_exp(e, Rc::clone(&env))).collect();
            Reference(Rc::new(RefCell::new(Vector(exprs))))
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
            let mut val = interp_exp(e1, Rc::clone(&env));
            let new_env: SymTable<String, Expr> = SymTable::extend(hashmap!(x => val), &env);
            return interp_exp(e2, Rc::new(new_env));
        } 
        If(box e, box e1, box e2) => 
            if interp_exp(e, Rc::clone(&env)) != Bool(false) { interp_exp(e1, env) } else { interp_exp(e2, env) },
        _ => panic!("bad syntax!"),
    }
}

fn interp_r2(expr: Expr) -> Expr {
    let env = Rc::new(SymTable::new());
    return interp_exp(expr, env);
}


// fn compile(expr: &str, filename: &str) -> std::io::Result<()> {
//     use crate::parser::parse;
//     use crate::compiler::*;
//     let expr = parse(expr);
//     // let expr = uniquify(expr);
//     let expr = remove_complex_opera(expr);
//     let expr = explicate_control(expr);
//     let expr = optimize_jumps(expr);
//     let expr = select_instruction(expr);
//     let expr = allocate_registers(expr);
//     let expr = patch_instructions(expr);
//     print_x86(expr, filename);
//     Ok(())
// }

fn main() -> std::io::Result<()> {
    use crate::compiler::*;
    use crate::typesystem::type_checker;
    use parser::parse;
    // let e = "(vector 42)";
    // let e = "(if (< (+ (+ 10 20) 16)
    //              42)
    //             10
    //             42000)";
    let e = "(let (x (vector 10 #f))
             (let (y (vector 42 x))
                (vector-ref (vector-ref y 1) 0)))";
    let e = parse(e);
    let e = type_checker(e);
    let mut locals = hashmap!();
    let e = expose_allocation(e, &mut locals);
    let e = remove_complex_opera(e);
    let e = explicate_control(e, locals);
    println!("locals: {:?}", e);
    Ok(())
}

                   