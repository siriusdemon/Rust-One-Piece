#[derive(Debug, PartialEq, Eq)]
pub enum Expr<'a> {
    Int { val: i64 },
    Prim { op: &'a str, args: Box<[Expr<'a>]> },
}

#[derive(Debug)]
pub struct Program<'a> {
    pub expr: Expr<'a>, 
}

