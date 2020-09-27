#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Expr {
    Int ( i64 ),
    Var ( String ),
    Let ( Box<Expr>, Box<Expr>,  Box<Expr> ),
    Prim ( String, Box<[Expr]> ),
}

#[derive(Debug)]
pub struct Program {
    pub expr: Expr, 
}


use std::collections::HashMap;
use std::rc::Rc;


// when I have time, when I reborn again, may I should pay a moment to this problem.
use std::hash::Hash;

#[derive(Clone, Debug)]
pub struct Environment<T, H> where T: Eq + Hash, H: Eq + Hash {
    pub map: HashMap<T, H>,
    env: Option<Rc<Environment<T, H>>>,
}

impl<T, H> Environment<T, H> where T: Eq + Hash, H: Eq + Hash {
    pub fn new() -> Self {
        Environment {
            map: HashMap::new(),
            env: None
        }
    }
    pub fn lookup(&self, x: &T) -> &H {
        if let Some(int) = self.map.get(x) {
            return int;
        } else if let Some(env) = &self.env {
            return env.lookup(x);
        } else {
            panic!("Undefine variable!");
        }
    }

    pub fn bind(&mut self, var: T, val: H) -> Option<H> {
        return self.map.insert(var, val); 
    }

    pub fn extend(self, map: HashMap<T, H>) -> Self {
        Environment { map, env: Some(Rc::new(self)) }
    }
}


// C0
#[derive(Debug, PartialEq, Eq, Clone, Hash)]
pub enum C0 {
    Int(i64),
    Var(String),
    Prim(String, Box<[C0]>),
    Assign(Box<C0>, Box<C0>),
    Return(Box<C0>),
    Seq(Box<C0>, Box<C0>),
}

#[derive(Debug)]
pub struct C0Program {
    pub locals: Vec<C0>,
    pub cfg: Vec<(String, C0)>, // control flow 
}

// x86
#[derive(Debug, PartialEq, Eq, Clone, Hash)]
pub enum x86 {
    RSP, RBP, RAX, RBX, RCX, RDX, RSI, RDI, 
    R8, R9, R10, R11, R12, R13, R14, R15,
    Imm(i64),
    Var(String),
    Deref(Box<x86>, i64),
    Instr(String, Box<[x86]>),
    Callq(String),
    Retq,
    Pushq(Box<x86>),
    Popq(Box<x86>),
    Jmp(String),
}

#[derive(Debug)]
pub struct x86Block {
    pub locals: Vec<x86>,
    pub instructions: Vec<x86>,
    pub stack_space: usize,
    pub name: String,
}

#[derive(Debug)]
pub struct x86Program {
    pub cfg: Vec<(String, x86Block)>, // control flow 
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
            Imm(n) => write!(f, "${}", n),
            Deref(box reg, n) => write!(f, "{}({})", n, reg),
            Instr(op, box [e]) => write!(f, "{} {}", op, e),
            Instr(op, box [e1, e2]) => write!(f, "{} {}, {}", op, e1, e2),
            Callq(function) => write!(f, "callq {}", function),
            Retq => write!(f, "retq"),
            Pushq(box reg) => write!(f, "pushq {}", reg),
            Popq(box reg) => write!(f, "popq {}", reg),
            Jmp(label) => write!(f, "jmp {}", label),
            e => panic!("invalid x86 code"),
        }
    }
}