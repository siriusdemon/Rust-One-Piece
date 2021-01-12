use crate::*;
use std::hash::Hash;

#[derive(PartialEq, Eq, Hash, Clone, Debug, Copy)]
pub enum RType {
    Integer,
    Boolean,
    Void,
    Vector,
}

use RType::*;

pub fn type_checker(expr: Expr) -> Expr {
    let env = Rc::new(SymTable::new());
    type_checker_expr(expr, env)
}

// Env store variable and its type
// war itself should be wrapped in Hastype
fn type_checker_expr(expr: Expr, env: Rc<SymTable<String, RType>>) -> Expr {
    match expr {
        Int(n) => Hastype(Box::new(Int(n)), Integer),
        Bool(b) => Hastype(Box::new(Bool(b)), Boolean),
        Expr::Void => Hastype(Box::new(Expr::Void), RType::Void),
        Var(x) => {
            let t = env.lookup(&x).clone();
            Hastype(Box::new(Var(x)), t)
        }
        Let(box Var(x), box e, box body) => {
            if let Hastype(box e, et) = type_checker_expr(e, Rc::clone(&env)) {
                let new_env = SymTable::extend(hashmap!(x.to_string() => et), &env);
                if let Hastype(box body, bt) = type_checker_expr(body, Rc::new(new_env)) {
                    let v = Hastype(Box::new(Var(x)), et);
                    let e = Hastype(Box::new(e), et);
                    let body = Hastype(Box::new(body), bt);
                    return Hastype(Box::new(
                                Let(Box::new(v), Box::new(e), Box::new(body))), 
                                bt);
                }
                panic!("Should not reach");
            }
            panic!("Should not reach");
        }
        If(box e, box e1, box e2) => {
            // here, we force every if expression should return same type
            let e = type_checker_expr(e, Rc::clone(&env));
            assert!(matches!(&e, Hastype(box _e, t) if t == &Boolean ));
            let e1 = type_checker_expr(e1, Rc::clone(&env));
            if let Hastype(e2, t2) = type_checker_expr(e2, env) {
                assert!(matches!(&e1, Hastype(box _e, t1) if t1 == &t2));
                let e2 = Hastype(e2, t2.clone());
                return Hastype(Box::new(If(Box::new(e), Box::new(e1), Box::new(e2))), t2);
            }
            unreachable!();
        }
        Prim0(op) if op.as_str() == "read" => Hastype(Box::new(Prim0(op)), Integer),
        Prim1(op, box e) => {
            let e = type_checker_expr(e, env);
            match op.as_str() {
                "-" => {
                    assert!(matches!(&e, Hastype(box _e, t) if t == &Integer));
                    Hastype(Box::new(Prim1(op, Box::new(e))), Integer)
                },
                "not" => {
                    assert!(matches!(&e, Hastype(box _e, t) if t == &Boolean));
                    Hastype(Box::new(Prim1(op, Box::new(e))), Boolean)
                },
                _ => panic!("unknown Prim1 operator"),
            }
        }
        Prim2(op, box e1, box e2) => {
            let e1 = type_checker_expr(e1, Rc::clone(&env));
            let e2 = type_checker_expr(e2, env);
            match op.as_str() {
                "and" | "or" => { 
                    assert!(matches!(&e1, Hastype(box _e, t) if t == &Boolean));
                    assert!(matches!(&e2, Hastype(box _e, t) if t == &Boolean));
                    return Hastype(Box::new(Prim2(op, Box::new(e1), Box::new(e2))), Boolean);
                }
                ">" | "<" | "<=" | ">=" | "eq?" => {
                    assert!(matches!(&e1, Hastype(box _e, t) if t == &Integer));
                    assert!(matches!(&e2, Hastype(box _e, t) if t == &Integer));
                    return Hastype(Box::new(Prim2(op, Box::new(e1), Box::new(e2))), Boolean);
                }
                "+" | "-" => {
                    assert!(matches!(&e1, Hastype(box _e, t) if t == &Integer));
                    assert!(matches!(&e2, Hastype(box _e, t) if t == &Integer));
                    return Hastype(Box::new(Prim2(op, Box::new(e1), Box::new(e2))), Integer);
                }
                "vector-ref" => {
                    assert!(matches!(&e1, Hastype(box _e, t) if t == &RType::Vector));
                    assert!(matches!(&e2, Hastype(box _e, t) if t == &Integer));
                    // here, we can return either vector int or bool
                    return Hastype(Box::new(Prim2(op, Box::new(e1), Box::new(e2))), Integer);
                }
                _ => panic!("unknown Prim2 operator"),
            }
        },
        Prim3(op, box e1, box e2, box e3) => {
            match op.as_str() {
                "vector-set!" => {
                    let e1 = type_checker_expr(e1, Rc::clone(&env));
                    let e2 = type_checker_expr(e2, Rc::clone(&env));
                    let e3 = type_checker_expr(e3, env);
                    assert!(matches!(&e1, Hastype(box _e, t) if t == &RType::Vector));
                    assert!(matches!(&e2, Hastype(box _e, t) if t == &RType::Integer));
                    // t3 is any type
                    return Hastype(Box::new(Prim3(op, Box::new(e1), Box::new(e2), Box::new(e3))), RType::Void);
                }
                e => {
                    println!("unknown Prim3 {}", e);
                    panic!("");
                }
            }
        }
        PrimN(op, mut exprs) if op.as_str() == "vector" => {
            exprs = exprs.into_iter().map(|e| type_checker_expr(e, Rc::clone(&env))).collect();
            return Hastype(Box::new(PrimN(op, exprs)), RType::Vector);
        }
        e => {
            println!("unsupported expr: {:?}", e);
            panic!("");
        }
    }
}