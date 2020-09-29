#![feature(box_patterns)]
mod syntax;
mod helper;
mod parser;
mod test;
mod compiler;

use std::rc::Rc;

pub use crate::syntax::Expr::{self, *};
pub use crate::syntax::SymTable;


fn interp_exp(env: Rc<SymTable<String, i64>>, expr: Expr) -> i64 {
    match expr {
        Int ( n ) => n,
        Prim0 ( op ) if op.as_str() == "read" => helper::readint(),
        Prim1 ( op , box e ) if op.as_str() == "-" => - interp_exp(env, e),
        Prim2 ( op, box e1, box e2) if op.as_str() == "+" => interp_exp(Rc::clone(&env), e1) + interp_exp(env, e2),
        Var ( x ) => *env.lookup(&x),
        Let (box Var ( x ), box e1, box e2) => {
            let val = interp_exp(Rc::clone(&env), e1);
            let mut new_env = SymTable::<String, i64>::extend(hashmap!(x => val), &env);
            return interp_exp(Rc::new(new_env), e2);
        } 
        _ => panic!("bad syntax!"),
    }
}

fn interp_r1(expr: Expr) -> i64 {
    let mut env = Rc::new(SymTable::new());
    return interp_exp(env, expr);
}

fn compile(expr: &str) -> std::io::Result<()> {
    use crate::parser::parse;
    use crate::compiler::*;
    let expr = parse(expr);
    let mut expr = uniquify(expr);
    let mut expr = remove_complex_opera(&mut expr);
    let expr = explicate_control(&mut expr);
    let expr = select_instruction(expr);
    let expr = assign_homes(expr);
    let expr = patch_instructions(expr);
    print_x86(expr, "r2.asm");
    Ok(())
}

fn main() -> std::io::Result<()> {
    let s = "(let (x 10) x)";
    compile(s)
}