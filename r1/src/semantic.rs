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

    pub fn bind(&mut self, var: T, val: H) {
        self.map.insert(var, val); 
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
    pub info: Environment<String, Vec<C0>>,
    pub cfg: Vec<(String, C0)>, // control flow 
}

// x86
#[derive(Debug, PartialEq, Eq, Clone, Hash)]
pub enum reg {
    rsp, rbp, rax, rbx, rcx, rdx, rsi, rdi, 
    r8, r9, r10, r11, r12, r13, r14, r15,
}

#[derive(Debug, PartialEq, Eq, Clone, Hash)]
pub enum arg {
    Reg(reg),
    Imm(i64),
    Var(String),
    Deref(reg, i64),
}

#[derive(Debug, PartialEq, Eq, Clone, Hash)]
pub enum x86 {
    Instr(String, Box<[arg]>),
    Callq(String),
    Retq,
    Pushq(arg),
    Popq(arg),
    Jmp(String),
}

#[derive(Debug)]
pub struct Block {
    info: Environment<String, x86>,
    instr: Vec<x86>,
}

#[derive(Debug)]
pub struct x86Program {
    pub info: Environment<String, Vec<x86>>,
    pub cfg: Vec<(String, Block)>, // control flow 
}