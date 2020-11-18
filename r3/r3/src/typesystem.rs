use crate::*;
use std::hash::Hash;

#[derive(PartialEq, Eq, Hash, Clone, Debug)]
pub enum RType {
    Integer,
    Boolean,
    Void,
    Vector,
}

use RType::*;

pub fn type_checker(expr: &Expr) -> RType {
    let env = Rc::new(SymTable::new());
    type_checker_expr(expr, env)
}

fn type_checker_expr(expr: &Expr, env: Rc<SymTable<String, RType>>) -> RType {
    match expr {
        Int(n) => Integer,
        Bool(b) => Boolean,
        Expr::Void => RType::Void,
        Var(x) => env.lookup(&x).clone(),
        Let(box Var(x), box e, box body) => {
            let etype = type_checker_expr(e, Rc::clone(&env));
            let new_env = SymTable::extend(hashmap!(x.to_string() => etype), &env);
            type_checker_expr(body, Rc::new(new_env))
        }
        If(box e, box e1, box e2) => {
            let etype = type_checker_expr(e, Rc::clone(&env));
            assert!(Boolean == etype);
            if e == &Bool(true) { type_checker_expr(e1, env) } else { type_checker_expr(e2, env) }
        }
        Prim0(op) if op.as_str() == "read" => Integer,
        Prim1(op, box e) => {
            let etype = type_checker_expr(e, env);
            match op.as_str() {
                "-" => assert!(Integer == etype),
                "not" => assert!(Boolean == etype),
                _ => panic!("unknown Prim1 operator"),
            };
            etype
        }
        Prim2(op, box e1, box e2) => {
            let e1type = type_checker_expr(e1, Rc::clone(&env));
            let e2type = type_checker_expr(e2, env);
            match op.as_str() {
                "and" | "or" => { 
                    assert!(Boolean == e1type); 
                    assert!(Boolean == e2type);
                    Boolean
                }
                ">" | "<" | "<=" | ">=" | "eq?" => {
                    assert!(Integer == e1type);
                    assert!(Integer == e2type);
                    Boolean
                }
                "+" | "-" => {
                    assert!(Integer == e1type);
                    assert!(Integer == e2type);
                    Integer
                }
                _ => panic!("unknown Prim2 operator"),
            }
        },
        Prim3(op, box e1, box e2, box e3) => {
            match op.as_str() {
                "vector-set!" => {
                    let t1 = type_checker_expr(e1, Rc::clone(&env));
                    let t2 = type_checker_expr(e2, Rc::clone(&env));
                    let t3 = type_checker_expr(e3, env);
                    assert!(RType::Vector == t1);
                    assert!(Integer == t2);
                    // t3 is any type
                    return RType::Void;
                }
                e => {
                    println!("unknown Prim3 {}", e);
                    panic!("");
                }
            }
        }
        Reference(r) => {
            let r = r.borrow();
            match &*r {
                Expr::Vector(ref _any) => RType::Vector, 
                _ => panic!("unknown reference type!"),
            }
        }
        e => {
            println!("unsupported expr: {:?}", e);
            panic!("");
        }
    }
}