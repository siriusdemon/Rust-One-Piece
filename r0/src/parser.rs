#[derive(Debug, Eq, PartialEq)]
pub enum Sexpr {
    Atom(String),
    List(Vec<Sexpr>)
}

pub use Sexpr::{Atom, List};
use crate::*;

pub fn scan(expr: &str) -> Sexpr {
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
            'a'..='z'|'A'..='Z' => sym.push(c),
            ' ' => if !sym.is_empty() { list.push(Atom(sym)); sym = String::new(); }
            ')' => {
                if !sym.is_empty() { list.push(Atom(sym)); sym = String::new(); }
                let mut nlist = stack.pop().unwrap();
                // here, start to build the first element to expr
                nlist.push(List(list));
                list = nlist;
            }
            _ => (),
        }
    }    
    if !sym.is_empty() {
        return Atom(sym);
    } 
    return list.pop().unwrap();
}

pub fn parse_sexpr(sexpr: &Sexpr) -> Expr {
    match sexpr {
        Atom(s) => {
            let val: i64 = s.parse().expect("Not an integer!");
            Int(val)
        },
        List(v) => {
            match v.as_slice() {
                [Atom(op)] if op.as_str() == "read" => Prim0 ( op.to_string() ),
                [Atom(op), e] if op.as_str() == "-" => Prim1 ( op.to_string(), Box::new(parse_sexpr(e))),
                [Atom(op), e1, e2] if op.as_str() == "+" => Prim2 ( op.to_string(), Box::new(parse_sexpr(e1)), Box::new(parse_sexpr(e2))),
                _ => panic!("Invalid form!"),
            }
        }
    }
}


pub fn parse(expr: &str) -> Expr {
    let sexpr = scan(expr);
    let expr = parse_sexpr(&sexpr);
    return expr;
}