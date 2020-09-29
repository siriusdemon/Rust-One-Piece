#[derive(Debug, Eq, PartialEq)]
pub enum Sexpr {
    Atom(String),
    List(Vec<Sexpr>)
}

pub use Sexpr::{Atom, List};

use crate::{Expr, Int, Prim0, Prim1, Prim2, Var, Let};
use crate::helper::is_digit;
use crate::string;

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

// syntax
// (let (a 10) (+ 10 a))
// (- 10 20)
pub fn parse_sexpr(sexpr: &Sexpr) -> Expr {
    match sexpr {
        Atom(s) => if is_digit(s) { Int(s.parse().unwrap())} else { Var(s.to_string()) }
        List(v) => match v.as_slice() {
            // let expression
            [Atom(_let), List(bind), exp] if _let.as_str() == "let" => {
                match bind.as_slice() {
                    [Atom(var), val] if !is_digit(var) => {
                        Let( Box::new(Var(var.to_string())), Box::new(parse_sexpr(val)), Box::new(parse_sexpr(exp)) )
                    },
                    _ => panic!("variable could not be digit!"),
                }
            },
            // prim
            [Atom(op), e1, e2] if op.as_str() == "+"    => Prim2 ( string!("+"), Box::new( parse_sexpr(e1)), Box::new(parse_sexpr(e2))),
            [Atom(op), e]      if op.as_str() == "-"    => Prim1 ( string!("-"), Box::new( parse_sexpr(e))),
            [Atom(op)]         if op.as_str() == "read" => Prim0 ( string!("read")),
            _ => panic!("Invalid syntax!"),
        }
    }
}


pub fn parse(expr: &str) -> Expr {
    let sexpr = parse_list(expr);
    let expr = parse_sexpr(&sexpr);
    return expr;
}