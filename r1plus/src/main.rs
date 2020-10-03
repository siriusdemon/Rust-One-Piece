#![feature(box_patterns)]
use std::rc::Rc;

mod syntax;
mod helper;
mod parser;
mod compiler;
mod test;

pub use crate::syntax::Expr::{self, *};
pub use crate::syntax::SymTable;


fn interp_exp(expr: Expr, env: Rc<SymTable<String, i64>>) -> i64 {
    match expr {
        Int ( n ) => n,
        Prim0 ( op ) if op.as_str() == "read" => helper::readint(),
        Prim1 ( op , box e ) if op.as_str() == "-" => -interp_exp(e, env),
        Prim2 ( op, box e1, box e2) if op.as_str() == "+" => interp_exp(e1, Rc::clone(&env)) + interp_exp(e2, env),
        Var ( x ) => *env.lookup(&x),
        Let (box Var ( x ), box e1, box e2) => {
            let val = interp_exp(e1, Rc::clone(&env));
            let new_env: SymTable<String, i64> = SymTable::extend(hashmap!(x => val), &env);
            return interp_exp(e2, Rc::new(new_env));
        } 
        _ => panic!("bad syntax!"),
    }
}

fn interp_r1(expr: Expr) -> i64 {
    let env = Rc::new(SymTable::new());
    return interp_exp(expr, env);
}

fn compile(expr: &str) -> std::io::Result<()> {
    use crate::parser::parse;
    use crate::compiler::*;
    let expr = parse(expr);
    let expr = uniquify(expr);
    let expr = remove_complex_opera(expr);
    let expr = explicate_control(expr);
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