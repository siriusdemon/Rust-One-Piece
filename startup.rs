#![feature(box_patterns)]


pub enum Expr<'a> {
    Int { val: i64 },
    Prim { op: &'a str, args: Box<[Expr<'a>]> },
}

pub struct Program<'a> {
    expr: Expr<'a>, 
}

use crate::Expr::{Int, Prim};
use std::io::{self, Write};


pub fn readint() -> i64 {
    print!("input an integer: ");
    match io::stdout().flush() {
        _ => (),
    }
    let mut v = String::new();
    io::stdin().read_line(&mut v)
        .expect("Failed to read line"); 
    match v.trim().parse() {
        Ok(num) => num,
        Err(_) => readint(),
    } 
}

fn is_expr(ast: &Expr) -> bool {
    match ast {
        Int { .. } => true,
        Prim { op: "read", args: box []} => true,
        Prim { op: "-", args: box [e]} => is_expr(e),
        Prim { op: "+", args: box [e1, e2]} => is_expr(e1) && is_expr(e2),
        _ => false,
    }
}

fn is_r0(ast: &Program) -> bool {
    match ast {
        Program { expr } => is_expr(&expr),
    }
}

fn interp_exp(expr: &Expr) -> i64 {
    match expr {
        Int { val } => *val,
        Prim { op: "read", args: box [] } => readint(),
        Prim { op: "-", args: box [e] } => 0 - interp_exp(e),
        Prim { op: "+", args: box [e1, e2] } => interp_exp(e1) + interp_exp(e2),
        _ => 42,
    }
}

fn interp_r0(p: &Program) -> i64 {
    match p {
        Program { expr } => interp_exp(expr),
    }
}

fn main()
{
    let eight = Expr::Int{ val: 8 };
    let e = Expr::Prim{ op: "-", args: Box::new([Expr::Int{val:16}])};
    println!("{}", is_expr(&eight));
    println!("{}", is_expr(&e));
    let prog = Program{ expr: e };
    println!("{}", is_r0(&prog));
    println!("{}", interp_r0(&prog));

    // create program by hand
    let p1 = Program {
        expr: Expr::Prim { op: "+", args: Box::new([Expr::Int{val:10}, Expr::Int{val:32}])}
    };
    println!("{}", interp_r0(&p1));
    
    // (+ 10 (- (+ 12 20)))
    let p2 = Program {
        expr: Prim { op: "+", 
                   args: Box::new([Int{val:10}, Prim{ op: "-", 
                                                    args: Box::new([Prim{ op: "+",
                                                                        args: Box::new([Int{val: 12}, Int{val: 20}])}])}])}
    };
    println!("{}", interp_r0(&p2));
    // (+ (read) 10)
    let p3 = Program {
        expr: Prim { op: "+",
                    args: Box::new([
                        Prim{ op: "read", args: Box::new([])}, 
                        Int {val: 32}
                    ])}
    };
    println!("{}", interp_r0(&p3));
}


// 如此痛苦，为什么不写一个 parser?

// cp1
// 这一节的主要内容是，从 racket 转 Rust
// 没有什么特别的

// 注意下，在后续的程序中，可能要修改到 ast，所以函数的签名可能会改动!