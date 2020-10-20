#![feature(box_patterns)]

use std::collections::{HashMap, HashSet};
use std::rc::Rc;
use std::hash::Hash;


#[derive(Debug, PartialEq, Eq, Clone, Hash)]
pub enum Expr {
    Int ( i64 ),
    Bool ( bool ),
    Var ( String ),
    Let ( Box<Expr>, Box<Expr>,  Box<Expr> ),
    If (Box<Expr>, Box<Expr>, Box<Expr>),
    Prim0 ( String ),
    Prim1 ( String, Box<Expr> ),
    Prim2 ( String, Box<Expr>, Box<Expr> ),
}


#[derive(Debug)]
pub struct Program {
    pub expr: Expr, 
}



#[derive(Debug)]
pub struct SymTable<T, H> where T: Eq + Hash, H: Eq + Hash {
    pub map: HashMap<T, H>,
    env: Option<Rc<SymTable<T, H>>>,
}

impl<T, H> SymTable<T, H> where T: Eq + Hash, H: Eq + Hash {
    pub fn new() -> Self {
        SymTable {
            map: HashMap::new(),
            env: None
        }
    }
    pub fn lookup(&self, x: &T) -> &H {
        if let Some(h) = self.map.get(x) {
            return h;
        } else if let Some(env) = &self.env {
            return env.lookup(x);
        } else {
            panic!("Undefine variable!");
        }
    }

    pub fn bind(&mut self, var: T, val: H) -> Option<H> {
        return self.map.insert(var, val); 
    }

    pub fn extend(map: HashMap<T, H>, table: &Rc<SymTable<T,H>>) -> Self {
        SymTable { map, env: Some(Rc::clone(table)) }
    }
}


// C1 -- Just keep C0 because I am lazy
#[derive(Debug, PartialEq, Eq, Clone, Hash)]
pub enum C0 {
    Int(i64),
    Var(String),
    Bool(bool),
    Prim0(String),
    Prim1(String, Box<C0>),
    Prim2(String, Box<C0>, Box<C0>),
    Assign(Box<C0>, Box<C0>),
    Return(Box<C0>),
    Seq(Box<C0>, Box<C0>),
    Goto(String),
    If(Box<C0>, Box<C0>, Box<C0>),
}

#[derive(Debug)]
pub struct C0Program {
    pub locals: HashSet<C0>,
    pub cfg: Vec<(String, C0)>, // control flow 
}

// x86
#[derive(Debug, PartialEq, Eq, Clone, Hash)]
pub enum x86 {
    RSP, RBP, RAX, RBX, RCX, RDX, RSI, RDI, 
    R8, R9, R10, R11, R12, R13, R14, R15,
    AL, AH, BL, BH, CL, CH, DL, DH,
    Imm(i64),
    Var(String),
    Deref(Box<x86>, i64),
    Op1(String, Box<x86>),
    Op2(String, Box<x86>, Box<x86>),
    Callq(String),
    Retq,
    Pushq(Box<x86>),
    Popq(Box<x86>),
    Jmp(String),
    Set(String, Box<x86>),
    Jmpif(String, String),
}

#[derive(Debug)]
pub struct x86Program {
    pub cfg: HashMap<String, Vec<x86>>, // control flow 
    pub locals: HashSet<x86>,
    pub stack_space: usize,
}

use std::fmt;
impl fmt::Display for x86 {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use x86::*;
        match self {
            RAX => write!(f, "%rax"), RBX => write!(f, "%rbx"), RCX => write!(f, "%rcx"), RDX => write!(f, "%rdx"), 
            RSI => write!(f, "%rsi"), RDI => write!(f, "%rdi"), RBP => write!(f, "%rbp"), RSP => write!(f, "%rsp"), 
            R8  => write!(f, "%r8"),  R9  => write!(f, "%r9"),  R10 => write!(f, "%r10"), R11 => write!(f, "%r11"), 
            R12 => write!(f, "%r12"), R13 => write!(f, "%r13"), R14 => write!(f, "%r14"), R15 => write!(f, "%r15"),
            AH => write!(f, "%ah"),   AL => write!(f, "%al"),   BH => write!(f, "%bh"),   BL => write!(f, "%bl"),
            CH => write!(f, "%ch"),   CL => write!(f, "%cl"),   DH => write!(f, "%dh"),   DL => write!(f, "%dl"),
            Imm(n) => write!(f, "${}", n),
            Deref(box reg, n) => write!(f, "{}({})", n, reg),
            Op1(op, box e) => write!(f, "{} {}", op, e),
            Op2(op, box e1, box e2) => write!(f, "{} {}, {}", op, e1, e2),
            Callq(function) => write!(f, "callq {}", function),
            Retq => write!(f, "retq"),
            Pushq(box reg) => write!(f, "pushq {}", reg),
            Popq(box reg) => write!(f, "popq {}", reg),
            Jmp(label) => write!(f, "jmp {}", label),
            Jmpif(cc, label) => write!(f, "j{} {}", cc, label),
            e => panic!("invalid x86 code"),
        }
    }
}


// 我怀着对这个世界的祝福苏醒