use std::collections::{HashSet, HashMap};


use crate::syntax::Expr::{self, *};
use crate::syntax::SymTable;
use crate::helper::gensym;
use crate::hashmap;

// ---------------------------------- shrink pass ----------------------------------------------
// maybe not be necessary
pub fn shrink(expr: Expr) -> Expr {
    match expr {
        Prim2(op, box e1, box e2) => {
            match op.as_str() {
                "-" => Prim2("+".to_string(), Box::new(e1), Box::new(Prim1("-".to_string(), Box::new(e2)))),
                "or" => {
                    let x = Var(gensym());
                    Let(Box::new(x.clone()), Box::new(e1),
                        Box::new(If(Box::new(x.clone()), Box::new(x), Box::new(e2))))
                }
                "and" => {
                    let x = Var(gensym());
                    Let(Box::new(x.clone()), Box::new(e1),
                        Box::new(If(Box::new(Prim1("not".to_string(), Box::new(x.clone()))), Box::new(x), Box::new(e2))))
                }
                ">=" => { // (not (< e1 e2))
                    let x = Var(gensym());
                    Let(Box::new(x.clone()), Box::new(e1),
                        Box::new(Prim1("not".to_string(), Box::new(Prim2("<".to_string(), Box::new(x), Box::new(e2))))))
                }
                "<=" => { // (not (< e2 e1)
                    let x = Var(gensym());
                    Let(Box::new(x.clone()), Box::new(e1),
                        Box::new(Prim1("not".to_string(), Box::new(Prim2("<".to_string(), Box::new(e2), Box::new(x))))))
                }
                ">" => { // (< e2 e1)
                    let x = Var(gensym());
                    Let(Box::new(x.clone()), Box::new(e2),
                        Box::new(Prim2("<".to_string(), Box::new(x), Box::new(e1))))
                }
                _ => Prim2(op, Box::new(shrink(e1)), Box::new(shrink(e2)))
            }
        }
        e => e
    }
}

// ---------------------------------- uniquify pass ---------------------------------------------
// make every variable name unique
use std::rc::Rc;
pub fn uniquify(expr: Expr) -> Expr {
    let symtable = Rc::new(SymTable::new());
    return uniquify_expr(expr, symtable);
}

fn uniquify_expr(expr: Expr, symtable: Rc<SymTable<String, String>>) -> Expr {
    match expr {
        Var(x) => Var(symtable.lookup(&x).to_string()),
        Int(n) => Int(n),
        Bool(b) => Bool(b),
        Let(box Var(x), box e, box body) => {
            let new_x = gensym();
            let new_symtable: SymTable<String, String> = SymTable::extend(hashmap!(x => new_x.clone()), &symtable);
            let new_e = Box::new(uniquify_expr(e, symtable));
            let new_body = Box::new(uniquify_expr(body, Rc::new(new_symtable)));
            return Let (Box::new(Var(new_x)), new_e, new_body);
        },
        Prim0(op) => Prim0(op),
        Prim1(op, box e1) => Prim1(op, Box::new( uniquify_expr(e1, symtable))),
        Prim2(op, box e1, box e2) => Prim2(op, Box::new(uniquify_expr(e1, Rc::clone(&symtable))), 
                                               Box::new(uniquify_expr(e2, symtable))),
        If(box e, box e1, box e2) => If(Box::new(uniquify_expr(e, Rc::clone(&symtable))), 
                                        Box::new(uniquify_expr(e1, Rc::clone(&symtable))),
                                        Box::new(uniquify_expr(e2, symtable))),
        _ => panic!("should not reach!"),
    }
}

// ---------------------------------- remove complex operands pass ----------------------------------
use std::mem;
pub fn remove_complex_opera(expr: Expr) -> Expr {
    match expr {
        Var(x) => Var(x),
        Int(n) => Int(n),
        Bool(b) => Bool(b),
        Let(box Var(x), box e, box body) => {
            Let( Box::new(Var(x)), Box::new( remove_complex_opera(e) ), Box::new(remove_complex_opera(body)))
        },
        If(box e, box e1, box e2) => {
            If(Box::new(remove_complex_opera(e)), Box::new(remove_complex_opera(e1)), Box::new(remove_complex_opera(e2)))
        },
        Prim0(op) => Prim0(op),
        Prim1(op, box e) => {
            if is_complex(&e) {
                let x = gensym();
                Let ( Box::new(Var(x.clone())), Box::new(remove_complex_opera(e)), Box::new(Prim1(op, Box::new(Var(x)))))
            } else {
                Prim1(op, Box::new( remove_complex_opera(e) ))
            }
        },
        Prim2(op, box e1, box e2) => {
            if is_complex(&e1) && is_complex(&e2) {
                let (x, y) = (gensym(), gensym());
                Let ( Box::new(Var(x.clone())), Box::new(remove_complex_opera(e1)), 
                    Box::new( Let ( Box::new(Var(y.clone())), Box::new(remove_complex_opera(e2)), 
                        Box::new(Prim2(op, Box::new(Var(x)), Box::new(Var(y)))))))
            } else if is_complex(&e1) {
                let x = gensym();
                Let ( Box::new(Var(x.clone())), Box::new(remove_complex_opera(e1)), 
                    Box::new(Prim2(op, Box::new(Var(x)), Box::new(remove_complex_opera(e2)))))
            } else if is_complex(&e2) {
                let x = gensym();
                Let ( Box::new(Var(x.clone())), Box::new(remove_complex_opera(e2)), 
                    Box::new(Prim2(op, Box::new(remove_complex_opera(e1)), Box::new(Var(x)))))
            } else {
                Prim2(op, Box::new( remove_complex_opera(e1)), Box::new(remove_complex_opera(e2) ))
            }
        },
        e => { println!("{:?}", e); panic!("should not reach!"); }
    }
}


fn is_complex(expr: &Expr) -> bool {
    let any_if = If(Box::new(Int(1)), Box::new(Int(1)), Box::new(Int(1))); 
    let any_let = Let(Box::new(Var(String::new())), Box::new(Int(1)), Box::new(Int(1)));
    let any_prim0 = Prim0(String::new());
    let any_prim1 = Prim1(String::new(), Box::new(Int(1)));
    let any_prim2 = Prim2(String::new(), Box::new(Int(1)), Box::new(Int(1)));
    mem::discriminant(expr) == mem::discriminant(&any_if)    ||
    mem::discriminant(expr) == mem::discriminant(&any_let)   ||
    mem::discriminant(expr) == mem::discriminant(&any_prim0) ||
    mem::discriminant(expr) == mem::discriminant(&any_prim1) ||
    mem::discriminant(expr) == mem::discriminant(&any_prim2) 
}

// ------------------------------ explicate control ------------------------
use crate::syntax::{C0, C0Program};
pub fn explicate_control(expr: Expr) -> C0Program {
    let mut cfg = HashMap::new();
    let mut locals = HashSet::new();
    let expr = explicate_tail(expr, &mut cfg, &mut locals);
    cfg.insert("start".to_string(), expr);
    C0Program { locals, cfg }
}

fn explicate_tail(expr: Expr, cfg: &mut HashMap<String, C0>, locals: &mut HashSet<C0>) -> C0 {
    match expr {
        Let(box Var(x), box e, box body) => {
            let tail = explicate_tail(body, cfg, locals);
            let tail = explicate_assign(C0::Var(x), e, tail, locals, cfg);
            return tail;
        }
        If(box e, box e1, box e2) => {
            let tail1 = explicate_tail(e1, cfg, locals);
            let tail2 = explicate_tail(e2, cfg, locals);
            let tail = explicate_pred(e, tail1, tail2, locals, cfg);
            return tail;
        }
        e => {
            return C0::Return(Box::new(expr_to_C0(e)));
        }
    }
}

fn explicate_assign(x: C0, expr: Expr, tail: C0, locals: &mut HashSet<C0>, cfg: &mut HashMap<String, C0>) -> C0 {
    use C0::{Assign, Seq};
    match expr {
        Let(box Var(x_), box e, box body) => {
            let tail = explicate_assign(x, body, tail, locals, cfg);
            let e = explicate_assign(C0::Var(x_), e, tail, locals, cfg);
            return e;
        },
        If(box e, box e1, box e2) => {
            // build block for tail
            let goto = attach_block(tail, cfg);
            let e1 = explicate_assign(x.clone(), e1, goto.clone(), locals, cfg);
            let e2 = explicate_assign(x, e2, goto, locals, cfg);
            return explicate_pred(e, e1, e2, locals, cfg);
        },
        e => {
            locals.insert(x.clone());
            let e = expr_to_C0(e);
            let assign = Assign(Box::new(x), Box::new(e));
            return Seq(Box::new(assign), Box::new(tail));
        }
    }
}

fn explicate_pred(cond: Expr, then: C0, else_: C0, locals: &mut HashSet<C0>, cfg: &mut HashMap<String, C0>) -> C0 {
    // e, boolean, if, or cmp
    match cond {
        Bool(true) => then,
        Bool(false) => else_,
        If(box e, box e1, box e2) => {
            // attach then and else as b1 b2 
            let goto1 = attach_block(then, cfg);
            let goto2 = attach_block(else_, cfg);
            let nthen = explicate_pred(e1, goto1.clone(), goto2.clone(), locals, cfg);
            let nelse = explicate_pred(e2, goto1, goto2, locals, cfg);
            let goto3 = attach_block(nthen, cfg);
            let goto4 = attach_block(nelse, cfg);
            return explicate_pred(e, goto3, goto4, locals, cfg);
        }
        Let(box Var(x), box e, box body) => {
            let tail = explicate_pred(body, then, else_, locals, cfg);
            return explicate_assign(C0::Var(x), e, tail, locals, cfg);
        }
        // cmp
        e => {
            let goto1 = attach_block(then, cfg);
            let goto2 = attach_block(else_, cfg); 
            C0::If(Box::new(expr_to_C0(e)), Box::new(goto1), Box::new(goto2))
        }
    }
}

fn expr_to_C0(expr: Expr) -> C0 {
    match expr {
        Int(n) => C0::Int(n),
        Var(x) => C0::Var(x),
        Bool(b) => C0::Bool(b),
        Prim0(op) => C0::Prim0(op),
        Prim1(op, box e) => C0::Prim1(op, Box::new( expr_to_C0(e) )),
        Prim2(op, box e1, box e2) => C0::Prim2(op, Box::new( expr_to_C0(e1) ), Box::new( expr_to_C0(e2) )),
        e => {
            println!("{:?}", e);
            panic!("No complex structure! Handle it by yourself!");
        }
    }
}


fn attach_block(expr: C0, cfg: &mut HashMap<String, C0>) -> C0 {
    let label = gensym();
    cfg.insert(label.clone(), expr);
    return C0::Goto(label);
}

// ----------------- optimize jump ----------------------------------------
// remove trivial block
fn get_shortcut(prog: &C0Program) -> HashMap<String, String> {
    // 跳往 key 的人，应该跳往 value
    let mut shortcut = HashMap::new();
    for (label, code) in &prog.cfg {
        if let C0::Goto(ref lab) = &code {
            let mut target = lab.clone();
            while let C0::Goto(ref lab_) = &prog.cfg.get(&target).unwrap() {
                target = lab_.to_string();
            }
            shortcut.insert(label.clone(), target);
        } 
    }
    return shortcut;
}


pub fn optimize_jumps(mut prog: C0Program) -> C0Program {
    use C0::*;
    let shortcut = get_shortcut(&prog);
    let C0Program { cfg, locals } = prog;
    let mut new_cfg = HashMap::new();
    for (label, code) in cfg.into_iter() {
        let code = optimize_jumps_helper(&label, code, &shortcut);
        // now, code should not be Just Goto!
        if label.as_str() == "start" || mem::discriminant(&code) != mem::discriminant(&Goto(String::new())) {
            new_cfg.insert(label, code);
        }
    }
    C0Program { cfg: new_cfg, locals }
}

fn optimize_jumps_helper(label: &String, code: C0, shortcut: &HashMap<String, String>) -> C0 {
    use C0::*;
    match code {
        Seq(box assign, box tail) => {
            let tail = optimize_jumps_helper(label, tail, shortcut);
            let seq = Seq(Box::new(assign), Box::new(tail));
            return seq;
        },
        Goto(lab) => match shortcut.get(&lab) {
            None => Goto(lab),
            Some(lab_) => Goto(lab_.to_string()),
        },
        If(box c, box e1, box e2) => {
            If(Box::new(c), Box::new(optimize_jumps_helper(label, e1, shortcut)),
                            Box::new(optimize_jumps_helper(label, e2, shortcut)))
        },
        e => e,
    }    
}

// ----------------- select instructions -----------------------------------
use crate::syntax::{x86, x86Program};
pub fn select_instruction(prog: C0Program) -> x86Program {
    let C0Program { locals, cfg } = prog;
    let cfg = cfg.into_iter()
        .map(|(label, code)|(label, C0_to_x86(code))).collect();
    let locals = locals.into_iter()
        .map(|x| simple_to_x86(x)).collect();
    x86Program { locals, cfg, stack_space: 0 }
}


pub fn C0_to_x86(expr: C0) -> Vec<x86> {
    let mut instr = vec![];
    C0_to_x86_helper(expr, &mut instr);
    return instr;
}

pub fn C0_to_x86_helper(expr: C0, code: &mut Vec<x86>) {
    use C0::*;
    match expr {
        Assign(box Var(x), box e) => assign_helper(e, x86::Var(x), code),
        Return(box e) => {
            assign_helper(e, x86::RAX, code);
            code.push( x86::Jmp("conclusion".to_string()) );
        }
        Seq(box assign, box tail) => {
            C0_to_x86_helper(assign, code);
            C0_to_x86_helper(tail, code);
        },
        If(box e, box Goto(e1), box Goto(e2)) => {
            pred_helper(e, e1, e2, code);
        },
        Goto(label) => {
            code.push( x86::Jmp(label) );
        },
        _ => panic!("bad syntax"),
    }
} 

fn pred_helper(expr: C0, then: String, else_: String, code: &mut Vec<x86>) {
    use C0::*;
    match expr {
        Var(x) => {
            code.push( x86::Op2("cmpq".to_string(), Box::new(x86::Imm(1)), Box::new(x86::Var(x))) );
            code.push( x86::Jmpif("e".to_string(), then) );
            code.push( x86::Jmp(else_) );
        }
        Prim2(op, box x, box y) => {
            code.push( x86::Op2("cmpq".to_string(), Box::new(simple_to_x86(y)), Box::new(simple_to_x86(x))) );
            match op.as_str() {
                "<"   => code.push( x86::Jmpif("l".to_string(), then) ),
                "eq?" => code.push( x86::Jmpif("e".to_string(), then) ),
                _ => panic!("unknown cmp op"),
            };
            code.push( x86::Jmp(else_) );
        }
        Prim1(op, box x) if op.as_str() == "not" => {
            code.push( x86::Op2("cmpq".to_string(), Box::new(x86::Imm(0)), Box::new(simple_to_x86(x))) );
            code.push( x86::Jmpif("e".to_string(), then) );
            code.push( x86::Jmp(else_) );
        }
        _ => unreachable!(),
    }
}

// 在发生赋值的时候使用
fn assign_helper(source: C0, target: x86, code: &mut Vec<x86>) {
    use C0::*;
    match source {
        Bool(true) => assign_helper(Int(1), target, code),
        Bool(false) => assign_helper(Int(0), target, code),
        Int(n) => code.push( x86::Op2("movq".to_string(), Box::new(x86::Imm(n)), Box::new(target))),
        Var(y) => code.push( x86::Op2("movq".to_string(), Box::new(x86::Var(y)), Box::new(target))),
        Prim0(read) if read.as_str() == "read" => {
            code.push( x86::Callq("read_int".to_string()));
            code.push( x86::Op2("movq".to_string(), Box::new(x86::RAX), Box::new(target)));
        },
        Prim2(op, box e1, box e2) if op.as_str() == "<" || op.as_str() == "eq?" => {
            code.push( x86::Op2("cmpq".to_string(), Box::new(simple_to_x86(e2)), Box::new(simple_to_x86(e1))) );
            let cc = if op.as_str() == "<" { String::from("l") } else { String::from("e") };
            code.push( x86::Set(cc, Box::new(x86::AL)));
            code.push( x86::Op2("movzbq".to_string(), Box::new(x86::AL), Box::new(target)));
        },
        Prim2(op, box e1, box e2) if op.as_str() == "+" => {
            match (e1, e2) {
                (Int(n1), Int(n2)) => {
                    code.push( x86::Op2("movq".to_string(), Box::new(x86::Imm(n1)), Box::new(target.clone())));
                    code.push( x86::Op2("addq".to_string(), Box::new(x86::Imm(n2)), Box::new(target)));
                },
                (Var(y), Int(n)) | (Int(n), Var(y)) => {
                    code.push( x86::Op2("movq".to_string(), Box::new(x86::Var(y)), Box::new(target.clone())));
                    code.push( x86::Op2("addq".to_string(), Box::new(x86::Imm(n)), Box::new(target)));
                },
                (Var(y), Var(z)) => {
                    match &target {
                        x86::Var(x) => {
                            if x.as_str() == y.as_str() {
                                code.push( x86::Op2("addq".to_string(), Box::new(x86::Var(z)), Box::new(target)));
                            } else if x.as_str() == z.as_str() {
                                code.push( x86::Op2("addq".to_string(), Box::new(x86::Var(y)), Box::new(target)));
                            } else {
                                code.push( x86::Op2("movq".to_string(), Box::new(x86::Var(y)), Box::new(target.clone())));
                                code.push( x86::Op2("addq".to_string(), Box::new(x86::Var(z)), Box::new(target)));
                            }
                        },
                        _ => { // 如果 target 是一个变量，就用上面的规则，否则，就用下面的规则
                            code.push( x86::Op2("movq".to_string(), Box::new(x86::Var(y)), Box::new(target.clone())));
                            code.push( x86::Op2("addq".to_string(), Box::new(x86::Var(z)), Box::new(target)));
                        }
                    }
                }
                _ => panic!("uncover"),
            }
        },
        Prim1(op, box e) => {
            match op.as_str() {
                "-" => {
                    code.push( x86::Op2("movq".to_string(), Box::new(simple_to_x86(e)), Box::new(target.clone())) );
                    code.push( x86::Op1("negq".to_string(), Box::new(target)));
                }
                "not" => {
                    code.push( x86::Op2("movq".to_string(), Box::new(simple_to_x86(e)), Box::new(target.clone())) );
                    code.push( x86::Op2("xorq".to_string(), Box::new(x86::Imm(1)), Box::new(target)) );
                }
                _ => panic!("bad syntax!"),
            }
        },
        _ => panic!("Invalid form for assignment!"),
    };
}

fn simple_to_x86(e: C0) -> x86 {
    use C0::*;
    match e {
        Int(n) => x86::Imm(n),
        Bool(true) => x86::Imm(1),
        Bool(false) => x86::Imm(0),
        Var(x) => x86::Var(x),
        _ => panic!("not simple C0, please handle by yourself!"),
    }
}

// --------------------------------- remove-jump ------------------------------------

// --------------------------------- uncover-live ------------------------------------
use crate::tsort;
pub fn uncover_live_prog(prog: &x86Program) -> HashMap<String, Vec<HashSet<&x86>>> {
    use x86::*;
    let cfg = &prog.cfg;
    let mapping:     HashMap<usize, &String> = cfg.keys().enumerate().collect();
    let mapping_rev: HashMap<&String, usize> = cfg.keys().zip(0..).collect();
    // 获取边和后继关系
    let mut successors = HashMap::new();
    let mut edges = vec![];
    for (label, instructions) in cfg {
        for instr in instructions.iter() {
            match instr {
                Jmp(label_) | Jmpif(_, label_) if label_.as_str() != "conclusion" => {
                    let from = *mapping_rev.get(label).unwrap();
                    let to = *mapping_rev.get(label_).unwrap();
                    edges.push( (from, to) );
                    // successors
                    let mut suc = successors.entry(from).or_insert(HashSet::new());
                    suc.insert(to);
                }
                _ => (),
            }
        }
    }
    let tsort_order = tsort::tsort(cfg.len(), edges);
    let order = tsort_order.into_iter().rev();

    // now we can deal with liveset with every block
    let mut livesets = HashMap::new();
    for id in order {
        let label = *mapping.get(&id).unwrap();
        let instructions = cfg.get(label).unwrap();
        // what we need is merge successors live after set
        let mut after: HashSet<&x86> = HashSet::new();
        if let Some(set) = successors.get(&id) {
            for successor_id in set {
                let set_: &Vec<HashSet<&x86>> = livesets.get(successor_id).unwrap();
                after = after.union(&set_[0]).cloned().collect();
            }
        }
        let liveset = uncover_live(instructions, after);
        livesets.insert(id, liveset);
    }
    // 每一个代码块都有它自己的 liveset
    let mut final_liveset = HashMap::new();
    for (id, liveset) in livesets {
        let label = mapping.get(&id).unwrap().clone().clone();
        final_liveset.insert(label, liveset);
    }
    return final_liveset;
}



pub fn uncover_live<'a>(instructions: &'a Vec<x86>, mut after: HashSet<&'a x86>) -> Vec<HashSet<&'a x86>> {
    let mut liveset = vec![];
    for code in instructions.iter().rev() {
        let (read, write) = compute_RW(&code);
        let mut before: HashSet<&x86> = after.difference(&write).cloned().collect();
        before = before.union(&read).cloned().collect();
        liveset.push(after);
        after = before;
    }
    let liveset = liveset.into_iter().rev().collect();
    return liveset;
}

// 要过滤掉那些整数以及寄存器
fn compute_RW(code: &x86) -> (HashSet<&x86>, HashSet<&x86>) {
    use x86::*;
    let mut read = HashSet::new();
    let mut write = HashSet::new();
    match code {
        Op1(op, box e) => {
            match op.as_str() {
                "negq" =>  if is_var(e)  { read.insert(e); write.insert(e); } 
                _ => unreachable!(),
            }
        },
        Op2(op, box e1, box e2) => {
            match op.as_str() {
                "addq" | "xorq" => {
                    if is_var(e1) { read.insert(e1); }
                    if is_var(e2) { read.insert(e2); write.insert(e2); } 
                },
                "movq" | "movzbq" => {
                    if is_var(e1) { read.insert(e1); }
                    if is_var(e2) { write.insert(e2); }
                },
                "cmpq" => {
                    if is_var(e1) { read.insert(e1); }
                    if is_var(e2) { read.insert(e2); }
                }
                _ => unreachable!(),
            }
        }
        _ => ()
    }
    return (read, write);
}

fn is_var(expr: &x86) -> bool {
    let any = x86::Var(String::new());
    mem::discriminant(expr) == mem::discriminant(&any)
}

// ----------------- build interference -----------------------------------
// 着色器算法
// 输入： locals, cfg, livesets
// 输出： 每个 locals 对应的寄存器标号 HashMap<x86, i8>
// 算法:
//      遍历 livesets 和 cfg，根据 op 的类型，记录变量间的关系
//      使用着色器分配算法，选出当前饱和度最高的一个变量
//      给当前变量分配一个最低的寄存器号，如果它有 move_related 的寄存器标号且不与它本身冲突，则使用之
//      更新与它相邻的其他变量的饱和度
//      重复以上三个步骤，直到所有的变量都完成分配
fn caller_saved_register() -> HashSet<x86> {
    use crate::hashset;
    hashset!(
        x86::RAX, x86::RDX, x86::RCX, x86::RDI, x86::RSI, 
        x86::R8,  x86::R9,  x86::R10, x86::R11
    )
}

fn callee_saved_register() -> HashSet<x86> {
    use crate::hashset;
    hashset!(
        x86::RSP, x86::RBP, x86::RBX, 
        x86::R12, x86::R13, x86::R14, x86::R15
    )
}


pub struct Graph {
    nodes: HashSet<x86>,                        // 图中节点
    move_related: HashMap<x86, x86>,            // 相互转移关系
    adjacent: HashMap<x86, HashSet<x86>>,       // 邻接表
    saturation: HashMap<x86, HashSet<i8>>,      // 每个节点的饱和度
    colors: HashMap<x86, i8>                    // 每个节点的颜色
}

impl Graph {

    pub fn new(nodes: &HashSet<x86>) -> Self {
        let nodes = nodes.clone();
        let move_related = HashMap::new();
        let adjacent = nodes.iter().map(|node| (node.clone(), HashSet::new())).collect();
        let saturation = nodes.iter().map(|node| (node.clone(), HashSet::new())).collect();
        let colors = nodes.iter().map(|node| (node.clone(), -1)).collect();
        Graph { nodes, move_related, adjacent, saturation, colors } 
    }

    pub fn add_edge(&mut self, v1: &x86, v2: &x86) {
        // now only allow variable, may cause problems when 
        // deal with register
        use x86::*;
        if is_var(v2) {
            let mut set1 = self.adjacent.get_mut(v1).unwrap();
            set1.insert(v2.clone());
        }
        if is_var(v1) {
            let mut set2 = self.adjacent.get_mut(v2).unwrap();
            set2.insert(v1.clone());
        }
    }

    fn most_saturation(&mut self) -> x86 {
        let mut most = 0;
        let mut k = &x86::Imm(42);
        for key in &self.nodes {
            let cur = self.saturation.get(key).unwrap().len();
            if cur >= most {
                k = key;
                most = cur; 
            }
        }
        assert_ne!(k, &x86::Imm(42));
        return k.clone();
    }

    fn find_min_available(&self, mut colors: Vec<i8>) -> i8 {
        let mut ret = 0;
        colors.sort();
        for c in colors.into_iter() {
            if ret < c {
                return ret;
            } else {
                ret = c + 1;
            }
        }
        return ret;
    }

    fn select_color(&self, node: &x86) -> i8 {
        let neighbors = self.adjacent.get(node).unwrap();
        let relate = self.try_move_relate(node);
        if relate != -1 { return relate; }
        let ncolors: Vec<i8> = neighbors.iter().map(|k| self.colors.get(k).unwrap().to_owned()).collect();
        self.find_min_available(ncolors)
    }

    fn try_move_relate(&self, node: &x86) -> i8 {
        let x = self.move_related.get(node);
        match x {
            None => -1,  // no relate
            Some(rel) => {
                let neighbors = self.adjacent.get(node).unwrap();
                if neighbors.contains(rel) { -1 } else { *self.colors.get(rel).unwrap() }
            }
        }
    }

    fn update_saturation(&mut self, node: &x86, color: i8) {
        let neighbors = self.adjacent.get(node).unwrap();
        for key in neighbors.iter() {
            let mut saturation = self.saturation.get_mut(key).unwrap();
            saturation.insert(color);
        }
    }

    pub fn colorize(&mut self) {
        while self.nodes.len() > 0 {
            let t = self.most_saturation();
            self.nodes.remove(&t);
            let color = self.select_color(&t);
            self.update_saturation(&t, color);
            self.colors.insert(t, color);
        }
    }
}


fn build_interference(block: &x86Program, livesets: HashMap<String, Vec<HashSet<&x86>>>) -> Graph {
    use x86::*;
    let mut graph = Graph::new(&block.locals);

    for (label, liveset) in livesets {
        let codes = block.cfg.get(&label).unwrap();
        for (code, set) in codes.iter().zip(liveset) {
            match &code {
                Op1(op, box e) => {
                    match op.as_str() {
                        "negq" => {
                            for &v in set.iter() {
                                if v == e { continue; }
                                graph.add_edge(v, e);
                            }
                        }
                        e => panic!("unknown instructions!"),
                    }
                },
                Op2(op, box e1, box e2) => {
                    match op.as_str() {
                        "addq" | "subq" | "cmpq" | "xorq" => {
                            for &v in set.iter() {
                                if v == e2 { continue; }
                                graph.add_edge(v, e2);
                            }
                        },
                        "movq" | "movzbq" => {
                            for &v in set.iter() {
                                if v == e2 || v == e1 { continue; }
                                graph.add_edge(e2, v);
                            }
                            if is_var(e1) && is_var(e2) {
                                graph.move_related.insert(e1.clone(), e2.clone());
                                graph.move_related.insert(e2.clone(), e1.clone());
                            }
                        },
                        e => panic!("unknown instructions!"),
                    }
                },
                Callq(label) => {
                    let saved_register = caller_saved_register();
                    for r in saved_register.iter() {
                        for &v in set.iter() {
                            graph.add_edge(r, v);
                        }
                    }
                },
                _ => (),
            }
        }
    }
    graph
}

// ---------------- color-graph -------------------------------------
fn register_map() -> HashMap<i8, x86> {
    hashmap!(
        0 => x86::RDX, 1 => x86::RCX, 2 => x86::RDI, 3 => x86::RSI,
        4 => x86::R8,  5 => x86::R9,  6 => x86::R10, 7 => x86::R11
    )
}


fn spill_register(colormap: HashMap<x86, i8>) -> (HashMap<x86, x86>, usize) {
    use x86::*;
    let regmap = register_map();
    let nums = regmap.len();
    let mut homes = HashMap::new();
    let mut n: usize = 0; // number on stacks
    for (k, v) in colormap {
        if homes.contains_key(&k) { continue; }
        let reg = regmap.get(&v);
        match reg {
            Some(r) => homes.insert(k, r.clone()),
            None => {
                let disp = Deref(Box::new(RBP), (n+1) as i64 * -8);
                n += 1;
                homes.insert(k, disp)
            },
        };
    }
    (homes, n)
}


fn color_graph(block: &x86Program) -> (HashMap<x86, x86>, usize) {
    let livesets = uncover_live_prog(&block);
    let mut graph = build_interference(block, livesets);
    graph.colorize();
    let colormap = graph.colors;
    spill_register(colormap) 
}

// ----------------- allocate-registers -----------------------------
const BYTE: usize = 8;
const FRAME: usize = 16;
pub fn allocate_registers(block: x86Program) -> x86Program {
    use x86::*;
    let (homesmap, n) = color_graph(&block);

    let x86Program { cfg, stack_space, locals } = block;
    let stack_space = align_address(n * BYTE, FRAME);

    fn helper(expr: x86, homesmap: &HashMap<x86, x86>) -> x86 {
        match expr {
            Var(x)  => homesmap.get(&Var(x)).unwrap().clone(),
            Op1(op, box e) => Op1(op, Box::new(helper(e, homesmap))),
            Op2(op, box e1, box e2) => Op2(op, Box::new(helper(e1, homesmap)), Box::new(helper(e2, homesmap))),
            e => e,
        }
    }
    let mut new_cfg = HashMap::new();
    for (label, mut codes) in cfg {
        codes = codes.into_iter().map(|code| helper(code, &homesmap)).collect(); 
        new_cfg.insert(label, codes);
    }
    x86Program { cfg: new_cfg, locals, stack_space }
}

#[inline]
fn align_address(space: usize, align: usize) -> usize {
    let remain = space % align;
    if remain == 0 { space } else { space + align - remain }
}


// ---------------------------------- patch instructions ---------------------------------------------
// You Should not call this pass directly, though, no harm.
pub fn patch_instructions(prog: x86Program) -> x86Program {
    use x86::*;
    let x86Program { locals, stack_space, cfg } = prog;
    let mut new_cfg = HashMap::new();
    for (label, codes) in cfg {
        let mut new_codes = vec![];
        for code in codes.into_iter() {
            match code {
                // move between two memory locations is not allowed
                Op2(op, box Deref(box reg1, n1), box Deref(box reg2, n2)) if op.as_str() == "movq" => {
                    new_codes.push( Op2(op.clone(), Box::new(Deref(Box::new(reg1), n1)), Box::new(RAX)));
                    new_codes.push( Op2(op, Box::new(RAX), Box::new(Deref(Box::new(reg2), n2))));
                },
                // unless move
                Op2(op, box r1, box r2) if op.as_str() == "movq" && r1 == r2 => (),
                // a little over engineering, remove useless add or sub
                Op2(op, box Imm(0), box r) if op.as_str() == "subq" || op.as_str() == "addq" => (),
                // fix cmpq for two immediate 
                Op2(op, box x, box Imm(n)) if op.as_str() == "cmpq" => {
                    new_codes.push( Op2("movq".to_string(), Box::new(Imm(n)), Box::new(RAX)));
                    new_codes.push( Op2(op, Box::new(x), Box::new(RAX)));
                }
                e => new_codes.push( e ),
            }
        }    
        new_cfg.insert(label, new_codes);
    }
    return x86Program { cfg: new_cfg, locals, stack_space };
}

// // ---------------------------------- print x86 ---------------------------------------------
use std::io::Write;
use std::fs::File;
pub fn print_x86(mut prog: x86Program, filename: &str) -> std::io::Result<()> {
    build_prelude(&mut prog);
    build_conclusion(&mut prog);
    prog = patch_instructions(prog);

    let mut file = File::create(filename)?;
    print_globl_entry(&mut file)?;
    print_x86program(prog, &mut file)?;
    return Ok(()); 
}


fn print_x86program(prog: x86Program, file: &mut File) -> std::io::Result<()> {
    for (label, codes) in prog.cfg {
        print_block(label, codes, file)?;
    }     
    Ok(())
}


fn print_block(label: String, codes: Vec<x86>, file: &mut File) -> std::io::Result<()> {
    file.write(label.as_bytes())?;
    file.write(b":\n")?;
    print_instructions(codes, file)?;
    file.write(b"\n")?;
    Ok(())
}

fn print_instructions(instructions: Vec<x86>, file: &mut File) -> std::io::Result<()> {
    use x86::*;
    for instr in instructions.into_iter() {
        let code = format!("    {}\n", instr);
        file.write(code.as_bytes())?;
    }
    return Ok(());
}

fn print_globl_entry(file: &mut File) -> std::io::Result<usize> {
    if std::env::consts::OS == "macos" {
        file.write(b".globl _main\n")
    } else {
        file.write(b".globl main\n")
    }
}

fn build_prelude(prog: &mut x86Program) {
    use x86::*;
    let label = "main".to_string();
    let instructions = vec![
        Pushq(Box::new(RBP)),
        Pushq(Box::new(R12)),
        Pushq(Box::new(R13)),
        Pushq(Box::new(R14)),
        Pushq(Box::new(RBX)),
        Op2("movq".to_string(), Box::new(RSP), Box::new(RBP)),
        Op2("subq".to_string(), Box::new(Imm(prog.stack_space as i64)), Box::new(RSP)),
        Jmp("start".to_string()),
    ];
    prog.cfg.insert(label, instructions);
}

fn build_conclusion(prog: &mut x86Program) {
    use x86::*;
    let label = "conclusion".to_string();
    let instructions = vec![
        Op2("addq".to_string(), Box::new(Imm(prog.stack_space as i64)), Box::new(RSP)),
        Popq(Box::new(RBX)),
        Popq(Box::new(R14)),
        Popq(Box::new(R13)),
        Popq(Box::new(R12)),
        Popq(Box::new(RBP)),
        Retq,
    ];
    prog.cfg.insert(label, instructions);
}

