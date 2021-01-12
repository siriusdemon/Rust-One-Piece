pub enum Expr {
    Num(f64),
    Var(String),
    Prim2(String, Box<Expr>, Box<Expr>),
    Call(String, Vec<Expr>),
    
}