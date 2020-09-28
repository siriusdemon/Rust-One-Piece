#[derive(Debug, PartialEq, Eq)]
pub enum Expr {
    Int(i64),
    Prim0(String),
    Prim1(String, Box<Expr>),
    Prim2(String, Box<Expr>, Box<Expr>),
}

pub use Expr::*;