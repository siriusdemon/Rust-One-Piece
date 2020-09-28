#[derive(Debug, PartialEq, Eq)]
pub enum Expr {
    Int(i64),
    Prim(String, Box<[Expr]>),
}

#[derive(Debug)]
pub struct Program {
    pub expr: Expr 
}

