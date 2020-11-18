#[derive(Debug, Eq, PartialEq)]
pub enum Sexpr {
    Atom(String),
    List(Vec<Sexpr>)
}

pub use Sexpr::{Atom, List};

use crate::*;
use crate::helper::*;
use crate::string;
use crate::typesystem::RType;

pub fn parse_list(expr: &str) -> Sexpr {
    let mut stack = vec![];
    let mut sym = String::new();
    let mut list = vec![];
    for c in expr.chars() {
        match c {
            '(' => { 
                stack.push(list); 
                list = vec![];
            }
            '0'..='9' => sym.push(c),
            '+'|'-'|'*'|'/' => sym.push(c),
            '<'|'='|'>'|'?'|'#'|'!' => sym.push(c),
            'a'..='z'|'A'..='Z' => sym.push(c),
            ' ' => if !sym.is_empty() { list.push(Atom(sym)); sym = String::new(); }
            ')' => {
                if !sym.is_empty() { list.push(Atom(sym)); sym = String::new(); }
                let mut nlist = stack.pop().unwrap();
                // here, start to build the first element to expr
                nlist.push(List(list));
                list = nlist;
                sym.clear();
            }
            _ => (),
        }
    } 
    if !sym.is_empty() {
        return Atom(sym);
    } else {
        return list.pop().unwrap();
    }
}

pub fn parse_sexpr(sexpr: &Sexpr) -> Expr {
    match sexpr {
        Atom(s) => {
            if is_digit(s) { Int(s.parse().unwrap())} 
            else if s == "#t" { Bool(true) }
            else if s == "#f" { Bool(false) }
            else { Var(s.to_string()) } 
        }
        List(v) => match v.as_slice() {
            // let expression
            [Atom(op), List(bind), exp] if op == "let" => {
                match bind.as_slice() {
                    [Atom(var), val] if !is_digit(var) => {
                        Let( Box::new(Var(var.to_string())), Box::new(parse_sexpr(val)), Box::new(parse_sexpr(exp)) )
                    },
                    _ => panic!("variable could not be digit!"),
                }
            },
            // if expression
            [Atom(op), e, e1, e2] if op == "if"
                => If ( Box::new( parse_sexpr(e)), Box::new(parse_sexpr(e1)), Box::new(parse_sexpr(e2))),
            // prim
            [Atom(op), e1, e2] if is_arithmetic(op) || is_cmp(op) || is_logical(op)
                => Prim2 ( op.to_string(), Box::new( parse_sexpr(e1)), Box::new(parse_sexpr(e2))),
            [Atom(op), e] if is_arithmetic(op) || is_logical(op) 
                => Prim1 (op.to_string(), Box::new( parse_sexpr(e))),
            [Atom(op)] if op == "read" => Prim0 ( string!("read")),
            // hastype
            [Atom(op), e, Atom(t)] if op == "has-type" => {
                let t = match t.as_str() {
                    "Integer" => RType::Integer,
                    "Boolean" => RType::Boolean,
                    "Vector" => RType::Vector,
                    "Void" => RType::Void,
                    _ => panic!("bad Type!"),
                };
                Hastype ( Box::new(parse_sexpr(e)),  t)
            }
            // vector
            [Atom(op), ref exps @ ..] if op == "vector" => {
                let mut v = vec![];
                for e in exps {
                    let e = parse_sexpr(e);
                    v.push(e);
                }
                Reference(Rc::new(RefCell::new(Vector(v))))
            }
            // vector-ref
            [Atom(op), e, index] if op == "vector-ref" => {
                let index = parse_sexpr(index);
                let e = parse_sexpr(e);
                Prim2("vector-ref".to_string(), Box::new(e), Box::new(index))
            }
            // vector-set!
            [Atom(op), e1, index, e2] if op == "vector-set!" => {
                let e1 = parse_sexpr(e1);
                let index = parse_sexpr(index);
                let e2 = parse_sexpr(e2);
                Prim3("vector-set!".to_string(), Box::new(e1), Box::new(index), Box::new(e2))
            }
            e => {
                println!("{:?}", e);
                panic!("Invalid syntax!");
            }
        }
    }
}


pub fn parse(expr: &str) -> Expr {
    let sexpr = parse_list(expr);
    let expr = parse_sexpr(&sexpr);
    return expr;
}