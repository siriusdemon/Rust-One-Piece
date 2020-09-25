/// make variable name unique
use crate::semantic::Expr::{self, *};
use crate::semantic::Environment;
use crate::helper::gensym;
use crate::hashmap;

// ---------------------------------- uniquify pass ---------------------------------------------
// make every variable name unique
pub fn uniquify(mut expr: Expr) -> Expr {
    let mut symtable = Environment::new();
    return uniquify_expr(&mut expr, &mut symtable);
}

fn uniquify_expr(expr: &mut Expr, symtable: &mut Environment<String, String>) -> Expr {
    match expr {
        Var(x) => Var(symtable.lookup(x).to_string()),
        Int(n) => Int(*n),
        Let(box Var(x), box e, box body) => {
            let new_x = gensym();
            let mut new_symtable = symtable.clone().extend(hashmap!(x.clone() => new_x.clone()));
            let new_e = Box::new(uniquify_expr(e, symtable));
            let new_body = Box::new(uniquify_expr(body, &mut new_symtable));
            return Let (Box::new(Var(new_x)), new_e, new_body);
        },
        Prim(op, box []) => Prim(op.to_string(), Box::new([])),
        Prim(op, box [e1]) => Prim(op.to_string(), Box::new([ uniquify_expr(e1, symtable)])),
        Prim(op, box [e1, e2]) => Prim(op.to_string(), Box::new([ uniquify_expr(e1, symtable), 
                                                                  uniquify_expr(e2, symtable)])),
        _ => panic!("should not reach!"),
    }
}

// ---------------------------------- remove complex operands pass ----------------------------------
use std::mem;
pub fn remove_complex_opera(expr: &mut Expr) -> Expr {
    let any_prim = Prim(String::new(), Box::new([]));
    match expr {
        Var(x) => Var(x.to_string()),
        Int(n) => Int(*n),
        Let(box Var(x), box e, box body) => {
            Let ( Box::new(Var(x.to_string())), Box::new( remove_complex_opera(e) ), Box::new( remove_complex_opera(body) ))
        },
        Prim(op, box []) => Prim(op.to_string(), Box::new([])),
        Prim(op, box [e]) => {
            if mem::discriminant(e) == mem::discriminant(&any_prim) {
                let x = gensym();
                Let ( Box::new(Var(x.clone())), Box::new(remove_complex_opera(e)), Box::new(Prim(op.to_string(), Box::new([Var(x)]))))
            } else {
                Prim(op.to_string(), Box::new([ remove_complex_opera(e) ]))
            }
        },
        Prim(op, box [e1, e2]) => {
            if mem::discriminant(e1) == mem::discriminant(&any_prim) {
                let x = gensym();
                Let ( Box::new(Var(x.clone())), Box::new(remove_complex_opera(e1)), 
                    Box::new(Prim(op.to_string(), Box::new([Var(x), remove_complex_opera(e2)]))))
            } else if mem::discriminant(e2) == mem::discriminant(&any_prim) {
                let x = gensym();
                Let ( Box::new(Var(x.clone())), Box::new(remove_complex_opera(e2)), 
                    Box::new(Prim(op.to_string(), Box::new([remove_complex_opera(e1), Var(x)]))))
            } else {
                Prim(op.to_string(), Box::new( [remove_complex_opera(e1), remove_complex_opera(e2)] ))
            }
        },
        _ => panic!("should not reach!"),
    }
}


// ------------------------------ explicate control ------------------------
use crate::semantic::{C0, C0Program};
pub fn explicate_control(expr: &mut Expr) -> C0Program {
    let expr = let_to_seq(expr);
    let expr = flatten_seq(expr);
    let mut info = Environment::new();
    let vars = collect_vars(&expr);
    info.bind("locals".to_string(), vars);
    C0Program { info, cfg: vec![("start".to_string(), expr)]}
}


pub fn let_to_seq(expr: &mut Expr) -> C0 {
    match expr {
        Int(n) => C0::Int(*n),
        Var(x) => C0::Var(x.to_string()),
        Prim(op, box []) => C0::Prim(op.to_string(), Box::new([])),
        Prim(op, box [e]) => C0::Prim(op.to_string(), Box::new([ let_to_seq(e) ])),
        Prim(op, box [e1, e2]) => {
            let e1 = let_to_seq(e1);
            let e2 = let_to_seq(e2);
            C0::Prim(op.to_string(), Box::new([e1, e2]))
        },
        Let(box Var(x), box e, box body) => {
            let assign = C0::Assign(Box::new(C0::Var(x.to_string())), Box::new(let_to_seq(e)));
            C0::Seq(Box::new(assign), Box::new(let_to_seq(body)))
        }
        _ => panic!("bad syntax"),
    }
}

pub fn flatten_seq(expr: C0) -> C0 {
    use C0::*;
    let mut stack = Vec::new();
    let mut tail = Return(Box::new(flatten_seq_helper(expr, &mut stack)));
    while let Some(assign) = stack.pop() {
        let seq = Seq(Box::new(assign), Box::new(tail));
        tail = seq; 
    }
    return tail;
}

pub fn flatten_seq_helper(expr: C0, stack: &mut Vec<C0>) -> C0 {
    use C0::*;
    let any_seq = Seq(Box::new(Int(1)), Box::new(Int(2)));
    match expr {
        Seq(box Assign(box x, box e), box tail) => {
            let assign = if mem::discriminant(&e) == mem::discriminant(&any_seq) {
                let e = flatten_seq_helper(e, stack);
                Assign(Box::new(x), Box::new(e))
            } else {
                Assign(Box::new(x), Box::new(e))
            };
            stack.push(assign);
            return flatten_seq_helper(tail, stack);
        }
        _ => expr,
    }
}

fn collect_vars(mut expr: &C0) -> Vec<C0> {
    use C0::*;
    let mut vars = vec![];
    while let Seq(box Assign(box x, box _e), box tail) = expr {
        vars.push(x.clone());
        expr = tail;
    }
    return vars;
}

// ----------------- select instructions -----------------------------------
use crate::semantic::{x86, x86Block, x86Program};
pub fn select_instruction(prog: C0Program) -> x86Block {
    let C0Program { info, mut cfg } = prog;
    let (label, codes_C0) = cfg.pop().unwrap();
    let mut instr = vec![];
    C0_to_x86(&codes_C0, &mut instr);
    let x86_block = x86Block { info: C0info_to_x86info(info), instr};
    return x86_block;
}

pub fn C0_to_x86(expr: &C0, code: &mut Vec<x86>) {
    use C0::*;
    match expr {
        Assign(box Var(x), box e) => assign_helper(e, x86::Var(x.to_string()), code),
        Return(box e) => {
            assign_helper(e, x86::RAX, code);
            code.push( x86::Jmp("conclusion".to_string()) );
        }
        Seq(box assign, box tail) => {
            C0_to_x86(assign, code);
            C0_to_x86(tail, code);
        },
        _ => panic!("bad syntax"),
    }
} 

fn assign_helper(source: &C0, target: x86, code: &mut Vec<x86>) {
    use C0::*;
    match source {
        Int(n) => code.push( x86::Instr("movq".to_string(), Box::new([x86::Imm(*n), target]))),
        Var(y) => code.push( x86::Instr("movq".to_string(), Box::new([x86::Var(y.to_string()), target]))),
        Prim(read, box []) if read.as_str() == "read" => {
                code.push( x86::Callq("read_int".to_string()));
                code.push( x86::Instr("movq".to_string(), Box::new([x86::RAX, target])));
            },
            Prim(add, box [e1, e2]) if add.as_str() == "+" => {
                match (e1, e2) {
                    (Int(n1), Int(n2)) => {
                        code.push( x86::Instr("movq".to_string(), Box::new([x86::Imm(*n1), target.clone()])));
                        code.push( x86::Instr("addq".to_string(), Box::new([x86::Imm(*n2), target])));
                    },
                    (Var(y), Int(n)) | (Int(n), Var(y)) => {
                        if let x86::Var(x) = &target {
                            if y.as_str() == x.as_str() {
                                code.push( x86::Instr("addq".to_string(), Box::new([x86::Imm(*n), target])));
                            } else {
                                code.push( x86::Instr("movq".to_string(), Box::new([x86::Imm(*n), target.clone()])));
                                code.push( x86::Instr("addq".to_string(), Box::new([x86::Var(y.to_string()), target])));
                            }
                        } else {
                            code.push( x86::Instr("movq".to_string(), Box::new([x86::Imm(*n), target.clone()])));
                            code.push( x86::Instr("addq".to_string(), Box::new([x86::Var(y.to_string()), target])));
                        }
                    },
                    (Var(y), Var(z)) => {
                        if let x86::Var(x) = &target {
                            if y.as_str() == x.as_str() {
                                code.push( x86::Instr("addq".to_string(), Box::new([x86::Var(z.to_string()), target])));
                            } else if z.as_str() == x.as_str() {
                                code.push( x86::Instr("addq".to_string(), Box::new([x86::Var(y.to_string()), target])));
                            } else {
                                code.push( x86::Instr("movq".to_string(), Box::new([x86::Var(y.to_string()), target.clone()])));
                                code.push( x86::Instr("addq".to_string(), Box::new([x86::Var(z.to_string()), target])));
                            }
                        } else {
                            code.push( x86::Instr("movq".to_string(), Box::new([x86::Var(y.to_string()), target.clone()])));
                            code.push( x86::Instr("addq".to_string(), Box::new([x86::Var(z.to_string()), target])));
                        }
                    }
                    _ => panic!("uncover"),
                }
            },
            Prim(neg, box [e]) if neg.as_str() == "-" => {
                match e {
                    Int(n) => {
                        code.push( x86::Instr("movq".to_string(), Box::new([x86::Imm(*n), target.clone()])) );
                        code.push( x86::Instr("negq".to_string(), Box::new([target])));
                    },
                    Var(y) => {
                        code.push( x86::Instr("movq".to_string(), Box::new([x86::Var(y.to_string()), target.clone()])) );
                        code.push( x86::Instr("negq".to_string(), Box::new([target])));
                    },
                    _ => panic!("bad syntax!"),
                };
            },
            _ => panic!("Invalid form for assignment!"),
        };
}


fn C0info_to_x86info(info: Environment<String, Vec<C0>>) -> Environment<String, Vec<x86>> {
    let mut new_info = Environment::new();
    for key in info.map.keys() {
        let mut new_vars = vec![];
        let values = info.map.get(key).unwrap();
        for x in values.iter() {
            if let C0::Var(x) = x {
                new_vars.push( x86::Var(x.to_string()));
            }
        }
        new_info.bind(key.to_string(), new_vars);
    }
    return new_info;
}