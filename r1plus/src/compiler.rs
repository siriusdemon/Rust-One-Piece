use crate::syntax::Expr::{self, *};
use crate::syntax::SymTable;
use crate::helper::gensym;
use crate::hashmap;

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
        _ => panic!("should not reach!"),
    }
}

// ---------------------------------- remove complex operands pass ----------------------------------
use std::mem;
pub fn remove_complex_opera(expr: Expr) -> Expr {
    match expr {
        Var(x) => Var(x),
        Int(n) => Int(n),
        Let(box Var(x), box e, box body) => {
            Let ( Box::new(Var(x)), Box::new( remove_complex_opera(e) ), Box::new( remove_complex_opera(body) ))
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
    let any_let = Let(Box::new(Var(String::new())), Box::new(Int(1)), Box::new(Int(1)));
    let any_prim0 = Prim0(String::new());
    let any_prim1 = Prim1(String::new(), Box::new(Int(1)));
    let any_prim2 = Prim2(String::new(), Box::new(Int(1)), Box::new(Int(1)));
    mem::discriminant(expr) == mem::discriminant(&any_let)   ||
    mem::discriminant(expr) == mem::discriminant(&any_prim0) ||
    mem::discriminant(expr) == mem::discriminant(&any_prim1) ||
    mem::discriminant(expr) == mem::discriminant(&any_prim2) 
}

// ------------------------------ explicate control ------------------------
use crate::syntax::{C0, C0Program};
pub fn explicate_control(expr: Expr) -> C0Program {
    let mut cfg = vec![];
    let mut locals = vec![];
    let expr = explicate_tail(expr, &mut cfg, &mut locals);
    cfg.push(("start".to_string(), expr));
    C0Program { locals, cfg }
}

fn explicate_tail(expr: Expr, cfg: &mut Vec<(String, C0)>, locals: &mut Vec<C0>) -> C0 {
    match expr {
        Let(box Var(x), box e, box body) => {
            let tail = explicate_tail(body, cfg, locals);
            let tail = explicate_assign(C0::Var(x), e, tail, locals, cfg);
            return tail;
        }
        e => {
            return C0::Return(Box::new(expr_to_C0(e)));
        }
    }
}

fn explicate_assign(x: C0, expr: Expr, tail: C0, locals: &mut Vec<C0>, cfg: &mut Vec<(String, C0)>) -> C0 {
    use C0::{Assign, Seq};
    match expr {
        Let(box Var(x_), box e, box body) => {
            let tail = explicate_assign(x, body, tail, locals, cfg);
            let e = explicate_assign(C0::Var(x_), e, tail, locals, cfg);
            return e;
        },
        e => {
            locals.push(x.clone());
            let e = expr_to_C0(e);
            let assign = Assign(Box::new(x), Box::new(e));
            return Seq(Box::new(assign), Box::new(tail));
        }
    }
}


fn expr_to_C0(expr: Expr) -> C0 {
    match expr {
        Int(n) => C0::Int(n),
        Var(x) => C0::Var(x),
        Prim0(op) => C0::Prim0(op),
        Prim1(op, box e) => C0::Prim1(op, Box::new( expr_to_C0(e) )),
        Prim2(op, box e1, box e2) => C0::Prim2(op, Box::new( expr_to_C0(e1) ), Box::new( expr_to_C0(e2) )),
        e => {
            println!("{:?}", e);
            panic!("No complex structure! Handle it by yourself!");
        }
    }
}

// ----------------- select instructions -----------------------------------
use crate::syntax::{x86, x86Block, x86Program};
pub fn select_instruction(prog: C0Program) -> x86Block {
    let C0Program { locals, mut cfg } = prog;
    let (label, codes_C0) = cfg.pop().unwrap();
    let instructions = C0_to_x86(codes_C0);
    let locals = vars_c0_to_x86(locals);
    let x86_block = x86Block { instructions, locals, stack_space: 0, name: "start".to_string() };
    return x86_block;
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
        _ => panic!("bad syntax"),
    }
} 

fn assign_helper(source: C0, target: x86, code: &mut Vec<x86>) {
    use C0::*;
    match source {
        Int(n) => code.push( x86::Op2("movq".to_string(), Box::new(x86::Imm(n)), Box::new(target))),
        Var(y) => code.push( x86::Op2("movq".to_string(), Box::new(x86::Var(y)), Box::new(target))),
        Prim0(read) if read.as_str() == "read" => {
            code.push( x86::Callq("read_int".to_string()));
            code.push( x86::Op2("movq".to_string(), Box::new(x86::RAX), Box::new(target)));
        },
        Prim2(add, box e1, box e2) if add.as_str() == "+" => {
            match (e1, e2) {
                (Int(n1), Int(n2)) => {
                    code.push( x86::Op2("movq".to_string(), Box::new(x86::Imm(n1)), Box::new(target.clone())));
                    code.push( x86::Op2("addq".to_string(), Box::new(x86::Imm(n2)), Box::new(target)));
                },
                (Var(y), Int(n)) | (Int(n), Var(y)) => {
                    if let x86::Var(x) = &target {
                        if y.as_str() == x.as_str() {
                            code.push( x86::Op2("addq".to_string(), Box::new(x86::Imm(n)), Box::new(target)));
                        } else {
                            code.push( x86::Op2("movq".to_string(), Box::new(x86::Imm(n)), Box::new(target.clone())));
                            code.push( x86::Op2("addq".to_string(), Box::new(x86::Var(y)), Box::new(target)));
                        }
                    } else {
                        code.push( x86::Op2("movq".to_string(), Box::new(x86::Imm(n)), Box::new(target.clone())));
                        code.push( x86::Op2("addq".to_string(), Box::new(x86::Var(y)), Box::new(target)));
                    }
                },
                (Var(y), Var(z)) => {
                    if let x86::Var(x) = &target {
                        if y.as_str() == x.as_str() {
                            code.push( x86::Op2("addq".to_string(), Box::new(x86::Var(z)), Box::new(target)));
                        } else if z.as_str() == x.as_str() {
                            code.push( x86::Op2("addq".to_string(), Box::new(x86::Var(y)), Box::new(target)));
                        } else {
                            code.push( x86::Op2("movq".to_string(), Box::new(x86::Var(y)), Box::new(target.clone())));
                            code.push( x86::Op2("addq".to_string(), Box::new(x86::Var(z)), Box::new(target)));
                        }
                    } else {
                        code.push( x86::Op2("movq".to_string(), Box::new(x86::Var(y)), Box::new(target.clone())));
                        code.push( x86::Op2("addq".to_string(), Box::new(x86::Var(z)), Box::new(target)));
                    }
                }
                _ => panic!("uncover"),
            }
        },
        Prim1(neg, box e) if neg.as_str() == "-" => {
            match e {
                Int(n) => {
                    code.push( x86::Op2("movq".to_string(), Box::new(x86::Imm(n)), Box::new(target.clone())) );
                    code.push( x86::Op1("negq".to_string(), Box::new(target)));
                },
                Var(y) => {
                    code.push( x86::Op2("movq".to_string(), Box::new(x86::Var(y)), Box::new(target.clone())) );
                    code.push( x86::Op1("negq".to_string(), Box::new(target)));
                },
                _ => panic!("bad syntax!"),
            };
        },
        _ => panic!("Invalid form for assignment!"),
        };
}


fn vars_c0_to_x86(locals: Vec<C0>) -> Vec<x86> {
    let mut new_vars = vec![];
    for x in locals.into_iter() {
        if let C0::Var(x) = x {
            new_vars.push( x86::Var(x) );
        }
    }
    return new_vars;
}

// --------------------------------- uncover-live ------------------------------------
use std::collections::HashSet;

pub fn uncover_live(instructions: &Vec<x86>) -> Vec<HashSet<&x86>> {
    let mut liveset = vec![];
    let mut after = HashSet::new();
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
        Op1(op, box e) if op.as_str() == "negq" => {
            if is_var(e) { read.insert(e); write.insert(e); }
        },
        Op2(op, box e1, box e2) if op.as_str() == "addq" => {
            if is_var(e1) { read.insert(e1); }
            if is_var(e2) { read.insert(e2); write.insert(e2); } 
        },
        Op2(op, box e1, box e2) if op.as_str() == "movq" => {
            if is_var(e1) { read.insert(e1); }
            if is_var(e2) { write.insert(e2); }
        },
        _ => ()
    }
    return (read, write);
}

fn is_var(expr: &x86) -> bool {
    let any = x86::Var(String::new());
    mem::discriminant(expr) == mem::discriminant(&any)
}

// ----------------- build interference -----------------------------------
use std::collections::HashMap;
pub struct Graph {
    move_related: HashMap<x86, x86>,
    nodes: HashSet<x86>,
    edges: HashMap<x86, HashSet<x86>>,
    colors: HashMap<x86, i8>,
    saturation: HashMap<x86, HashSet<i8>>,
}

impl Graph {
    pub fn new() -> Self {
        Graph { 
            nodes: HashSet::new(), edges: HashMap::new(), colors: HashMap::new(), 
            saturation: HashMap::new(), move_related: HashMap::new(),
        } 
    }
    #[inline]
    fn add_node(&mut self, node: x86) -> bool {
        self.nodes.insert(node)
    }
    pub fn add_edge(&mut self, v1: &x86, v2: &x86) {
        use x86::*;
        if v2 != &x86::RAX {
            let mut set1 = self.edges.entry(v1.clone()).or_insert(HashSet::new());
            set1.insert(v2.clone());
        }
        if v1 != &x86::RAX {
            let mut set2 = self.edges.entry(v2.clone()).or_insert(HashSet::new());
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
        k.clone()
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
        let neighbors = self.edges.get(node);
        match neighbors {
            None => 0,
            Some(neighbors) => {
                let relate = self.try_move_relate(node);
                if relate != -1 { return relate; }
                let ncolors: Vec<i8> = neighbors.iter().map(|k| self.colors.get(k).unwrap().to_owned()).collect();
                self.find_min_available(ncolors)
            }
        }
    }
    fn try_move_relate(&self, node: &x86) -> i8 {
        let x = self.move_related.get(node);
        match x {
            None => -1,  // no relate
            Some(rel) => {
                match self.edges.get(node) {
                    None => -1,
                    Some(set) => if set.contains(rel) { -1 } else { *self.colors.get(rel).unwrap() }
                }
            }
        }
    }
    fn init_saturation(&mut self) {
        for node in &self.nodes {
            self.saturation.entry(node.clone()).or_insert(HashSet::new());
        }
    }
    fn init_colors(&mut self) {
        for node in &self.nodes {
            self.colors.entry(node.clone()).or_insert(-1);
        }
    }
    fn update_saturation(&mut self, node: &x86, color: i8) {
        match self.edges.get(node) {
            None => (),
            Some(neighbors) => {
                for key in neighbors.iter() {
                    let mut saturation = self.saturation.get_mut(key).unwrap();
                    saturation.insert(color);
                }
            }
        }
    }
    pub fn colorize(&mut self) {
        self.init_saturation();
        self.init_colors();
        // println!("move related {:?}", self.move_related);
        // println!("edges {:?}", self.edges.keys());
        while self.nodes.len() > 0 {
            let t = self.most_saturation();
            self.nodes.remove(&t);
            let color = self.select_color(&t);
            self.update_saturation(&t, color);
            self.colors.insert(t, color);
        }
    }
}

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

fn build_interference(block: &x86Block, liveset: Vec<HashSet<&x86>>) -> Graph {
    use x86::*;
    let mut graph = Graph::new();

    for var in &block.locals {
        graph.add_node(var.clone());
    }

    let saved_register = caller_saved_register();
    for (code, set) in block.instructions.iter().zip(liveset) {
        match code {
            Op1(op, box e) if is_arithmetic(op.as_str()) => {
                for &v in set.iter() {
                    if v == e { continue; }
                    graph.add_edge(v, e);
                }
            },
            Op2(op, box e1, box e2) if is_arithmetic(op.as_str()) => {
                for &v in set.iter() {
                    if v == e2 { continue; }
                    graph.add_edge(v, e2);
                }
            },
            Op2(op, box e1, box e2) if op.as_str() == "movq" => {
                for &v in set.iter() {
                    if v == e2 || v == e1 { continue; }
                    graph.add_edge(e2, v);
                    // for move relate
                }
                if is_var(e1) && is_var(e2) {
                    graph.move_related.insert(e1.clone(), e2.clone());
                    graph.move_related.insert(e2.clone(), e1.clone());
                }
            },
            Callq(label) => {
                for r in saved_register.iter() {
                    for &v in set.iter() {
                        graph.add_edge(r, v);
                    }
                }
            },
            _ => (),
        }
    }
    graph
}

fn is_arithmetic(op: &str) -> bool {
    op == "negq" || op == "addq" || op == "subq" || op == "idivq"
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


fn color_graph(block: &x86Block) -> (HashMap<x86, x86>, usize) {
    let liveset = uncover_live(&block.instructions);
    let mut graph = build_interference(block, liveset);
    graph.colorize();
    let colormap = graph.colors;
    spill_register(colormap) 
}

// ----------------- allocate-registers -----------------------------
const BYTE: usize = 8;
const FRAME: usize = 16;
pub fn allocate_registers(block: x86Block) -> x86Block {
    use x86::*;
    let (homesmap, n) = color_graph(&block);

    let x86Block { instructions, locals, stack_space, name } = block;
    let stack_space = align_address(n * BYTE, FRAME);

    fn helper(expr: x86, homesmap: &HashMap<x86, x86>) -> x86 {
        match expr {
            Var(x)  => homesmap.get(&Var(x)).unwrap().clone(),
            Op1(op, box e) => Op1(op, Box::new(helper(e, homesmap))),
            Op2(op, box e1, box e2) => Op2(op, Box::new(helper(e1, homesmap)), Box::new(helper(e2, homesmap))),
            e => e,
        }
    }
    let instructions = instructions.into_iter().map(|code| helper(code, &homesmap)).collect();
    x86Block { instructions, locals, stack_space, name }
}

#[inline]
fn align_address(space: usize, align: usize) -> usize {
    let remain = space % align;
    if remain == 0 { space } else { space + align - remain }
}


// ---------------------------------- patch instructions ---------------------------------------------
pub fn patch_instructions(block: x86Block) -> x86Block {
    use x86::*;
    let x86Block { locals, instructions, stack_space, name } = block;
    let mut new_instructions = vec![];
    for instr in instructions.into_iter() {
        match instr {
            Op2(op, box Deref(box reg1, n1), box Deref(box reg2, n2)) if op.as_str() == "movq" => {
                new_instructions.push( Op2(op.clone(), Box::new(Deref(Box::new(reg1), n1)), Box::new(RAX)));
                new_instructions.push( Op2(op, Box::new(RAX), Box::new(Deref(Box::new(reg2), n2))));
            },
            Op2(op, box r1, box r2) if op.as_str() == "movq" && r1 == r2 => (),
            // a little over engineering
            Op2(op, box Imm(0), box r) if op.as_str() == "subq" || op.as_str() == "addq" => (),
            e => new_instructions.push( e ),
        }
    }
    return x86Block { locals, instructions: new_instructions, stack_space, name };
}

// ---------------------------------- print x86 ---------------------------------------------
use std::io::Write;
use std::fs::File;
pub fn print_x86(block: x86Block, filename: &str) -> std::io::Result<()> {
    let mut prelude = build_prelude(block.stack_space, &block.name);
    prelude = patch_instructions(prelude);
    let mut conclusion = build_conclusion(block.stack_space);
    conclusion = patch_instructions(conclusion);

    let mut file = File::create(filename)?;
    print_globl_entry(&mut file)?;
    print_block(prelude, &mut file)?;
    print_block(block, &mut file)?;
    print_block(conclusion, &mut file)?;
    return Ok(()); 
}

fn print_block(block: x86Block, file: &mut File) -> std::io::Result<()> {
    let x86Block { locals, instructions, stack_space, name } = block;
    file.write(name.as_bytes())?;
    file.write(b":\n")?;
    print_instructions(instructions, file)?;
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

fn build_prelude(stack_space: usize, jump_to: &String) -> x86Block {
    use x86::*;
    let name = "main".to_string();
    let instructions = vec![
        Pushq(Box::new(RBP)),
        Op2("movq".to_string(), Box::new(RSP), Box::new(RBP)),
        Op2("subq".to_string(), Box::new(Imm(stack_space as i64)), Box::new(RSP)),
        Jmp(jump_to.to_string()),
    ];
    x86Block {name, instructions, stack_space:0, locals: vec![]}
}

fn build_conclusion(stack_space: usize) -> x86Block {
    use x86::*;
    let name = "conclusion".to_string();
    let instructions = vec![
        Op2("addq".to_string(), Box::new(Imm(stack_space as i64)), Box::new(RSP)),
        Popq(Box::new(RBP)),
        Retq,
    ];
    x86Block {name, instructions, stack_space:0, locals:vec![]}
}

