#![feature(box_patterns)]
mod semantic;
mod helper;
mod parser;
mod test;
mod compiler;


pub use crate::semantic::Expr::{self, *};
use crate::semantic::Environment;
use crate::parser::parse;

// so, there is a mistake that I can copy type [expr]
fn interp_exp(env: &mut Environment<String, i64>, expr: &Expr) -> i64 {
    match expr {
        Int ( n ) => *n,
        Prim ( _read, box [] ) => helper::readint(),
        Prim ( _sub , box [e] ) => 0 - interp_exp(env, e),
        Prim ( _add, box [e1, e2]) => interp_exp(env, e1) + interp_exp(env, e2),
        Var ( x ) => *env.lookup(x),
        Let (box Var ( x ), box e1, box e2) => {
            let val = interp_exp(env, e1);
            let mut new_env = env.clone().extend(hashmap!(x.to_string() => val));
            return interp_exp(&mut new_env, e2);
        } 
        _ => panic!("bad syntax!"),
    }
}

fn interp_r1(expr: Expr) -> i64 {
    let mut env = Environment::new();
    return interp_exp(&mut env, &expr);
}

fn main() -> std::io::Result<()> {
    use crate::semantic::x86;
    let n = x86::Imm(10);
    println!("{}", n);
    let deref = x86::Deref(Box::new(x86::RBP), 10);
    println!("{}", deref);
    let instr = x86::Instr("movq".to_string(), Box::new([n, deref]));
    println!("{}", instr);
    let call = x86::Callq("read_int".to_string());
    println!("{}", call);
    let push = x86::Pushq(Box::new(x86::RBP));
    println!("{}", push);
    let pop = x86::Popq(Box::new(x86::RSP));
    println!("{}", pop);
    let var = x86::Var("haha".to_string());
    println!("{}", var);
    Ok(())
}