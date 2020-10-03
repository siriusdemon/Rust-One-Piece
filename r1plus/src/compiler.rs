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
            if is_prim1_or_prim2(&e) {
                let x = gensym();
                Let ( Box::new(Var(x.clone())), Box::new(remove_complex_opera(e)), Box::new(Prim1(op, Box::new(Var(x)))))
            } else {
                Prim1(op, Box::new( remove_complex_opera(e) ))
            }
        },
        Prim2(op, box e1, box e2) => {
            if is_prim1_or_prim2(&e1) {
                let x = gensym();
                Let ( Box::new(Var(x.clone())), Box::new(remove_complex_opera(e1)), 
                    Box::new(Prim2(op, Box::new(Var(x)), Box::new(remove_complex_opera(e2)))))
            } else if is_prim1_or_prim2(&e2) {
                let x = gensym();
                Let ( Box::new(Var(x.clone())), Box::new(remove_complex_opera(e2)), 
                    Box::new(Prim2(op, Box::new(remove_complex_opera(e1)), Box::new(Var(x)))))
            } else {
                Prim2(op, Box::new( remove_complex_opera(e1)), Box::new(remove_complex_opera(e2) ))
            }
        },
        _ => panic!("should not reach!"),
    }
}


fn is_prim1_or_prim2(expr: &Expr) -> bool {
    let any_prim1 = Prim1(String::new(), Box::new(Int(1)));
    let any_prim2 = Prim2(String::new(), Box::new(Int(1)), Box::new(Int(1)));
    mem::discriminant(expr) == mem::discriminant(&any_prim1) ||
    mem::discriminant(expr) == mem::discriminant(&any_prim2)
}

// ------------------------------ explicate control ------------------------
use crate::syntax::{C0, C0Program};
pub fn explicate_control(expr: Expr) -> C0Program {
    let expr = let_to_seq(expr);
    let expr = flatten_seq(expr);
    let locals = collect_vars(&expr);
    C0Program { locals, cfg: vec![("start".to_string(), expr)]}
}


pub fn let_to_seq(expr: Expr) -> C0 {
    match expr {
        Int(n) => C0::Int(n),
        Var(x) => C0::Var(x),
        Prim0(op) => C0::Prim0(op),
        Prim1(op, box e) => C0::Prim1(op, Box::new( let_to_seq(e) )),
        Prim2(op, box e1, box e2) => C0::Prim2(op, Box::new( let_to_seq(e1) ), Box::new( let_to_seq(e2) )),
        Let(box Var(x), box e, box body) => {
            let assign = C0::Assign(Box::new(C0::Var(x)), Box::new(let_to_seq(e)));
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
use crate::syntax::{x86, x86Block, x86Program};
pub fn select_instruction(prog: C0Program) -> x86Block {
    let C0Program { locals, mut cfg } = prog;
    let (label, codes_C0) = cfg.pop().unwrap();
    let instructions = C0_to_x86(codes_C0);
    let x86_block = x86Block { instructions, locals: vars_c0_to_x86(locals), stack_space: 0, name: "start".to_string() };
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
    for x in locals.iter() {
        if let C0::Var(x) = x {
            new_vars.push( x86::Var(x.to_string()));
        }
    }
    return new_vars;
}

// --------------------------------- uncover-live ------------------------------------
use std::collections::HashSet;
pub fn uncover_live(block: x86Block) -> x86Block {
    let x86Block { instructions, stack_space, locals, name } = block;
    let liveset = uncover_live_helper(&instructions);

    x86Block { instructions, stack_space, locals, name }
}

pub fn uncover_live_helper(instructions: &Vec<x86>) -> Vec<HashSet<&x86>> {
    let mut liveset = vec![];
    let mut after = HashSet::new();
    for code in instructions.iter().rev() {
        let (read, write) = compute_RW(&code);
        let mut before: HashSet<&x86> = after.difference(&write).cloned().collect();
        before = before.union(&read).cloned().collect();
        liveset.push(after);
        after = before;
    }
    return liveset;
}

fn compute_RW(code: &x86) -> (HashSet<&x86>, HashSet<&x86>) {
    use x86::*;
    let mut read = HashSet::new();
    let mut write = HashSet::new();
    match code {
        Op1(op, box e) if op.as_str() == "negq" => {
            read.insert(e); 
            write.insert(e);
        },
        Op2(op, box e1, box e2) if op.as_str() == "addq" => {
            if mem::discriminant(e1) == mem::discriminant(&Var(String::new())) {
                read.insert(e1);
            }
            read.insert(e2);
            write.insert(e2);
        },
        Op2(op, box e1, box e2) if op.as_str() == "movq" => {
            if mem::discriminant(e1) == mem::discriminant(&Var(String::new())) {
                read.insert(e1);
            }
            write.insert(e2);
        },
        _ => ()
    }
    return (read, write);
}

// ----------------- build interference -----------------------------------
use std::collections::HashMap;
pub struct Graph {
    nodes: HashSet<String>,
    edges: HashMap<String, HashSet<String>>,
    colors: HashMap<String, i8>,
    saturation: HashMap<String, HashSet<i8>>,
}

impl Graph {
    pub fn new() -> Self {
        Graph { nodes: HashSet::new(), edges: HashMap::new(), colors: HashMap::new(), saturation: HashMap::new() } 
    }
    #[inline]
    fn add_node(&mut self, node: String) -> bool {
        self.nodes.insert(node)
    }
    pub fn add_edge(&mut self, v1: &x86, v2: &x86) {
        use x86::*;
        match (v1, v2) {
            (Var(x1), Var(x2)) => {
                self.add_node(x1.to_string());
                self.add_node(x2.to_string());
                let mut set1 = self.edges.get_mut(x1).unwrap();
                set1.insert(x2.to_string());
                let mut set2 = self.edges.get_mut(x2).unwrap();
                set2.insert(x1.to_string());
            },
            (Var(x), reg) | (reg, Var(x)) => {
                self.add_node(x.to_string());
                let r = format!("{}", reg);
                self.add_node(r.clone());
                let mut set = self.edges.get_mut(&r).unwrap();
                set.insert(x.to_string());
                let mut set = self.edges.get_mut(x).unwrap();
                set.insert(r);
            },
            _ => panic!("add edge error"),
        }
    }
    fn most_saturation(&self) -> String {
        let mut most = 0;
        let mut k = &String::new();
        for key in self.saturation.keys() {
            let cur = self.saturation.get(key).unwrap().len();
            if cur >= most {
                k = key;
                most = cur; 
            }
        }
        k.to_string()
    }
    fn select_color(&self, node: &String) -> i8 {
        // 选出所有与 node 相邻的点，并看看它们都有什么颜色了，选出他们没有的最低的颜色
        let neighbors = self.edges.get(node).unwrap();
        let mut ncolors: Vec<i8> = neighbors.iter().map(|k| self.colors.get(k).unwrap().to_owned()).collect();
        ncolors.sort();
        let mut color = ncolors[0];
        for c in ncolors.into_iter() {
            if c == color {
                color = c + 1;
            }
        }
        return color;
    }
    fn update_saturation(&mut self, node: &String, color: i8) {
        let neighbors = self.edges.get(node).unwrap();
        for key in neighbors.iter() {
            let mut saturation = self.saturation.get_mut(key).unwrap();
            saturation.insert(color);
        }
    }
    pub fn colorize(&mut self) {
        for _ in 0..self.nodes.len() {
            let t = self.most_saturation();
            let color = self.select_color(&t);
            self.update_saturation(&t, color);
            self.colors.insert(t, color);
        }
    }
    pub fn colormap(self) -> HashMap<String, i8> {
        self.colors
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

fn build_interference(instructions: &Vec<x86>, liveset: Vec<HashSet<&x86>>) -> Graph {
    use x86::*;
    let mut graph = Graph::new();
    let saved_register = caller_saved_register();
    for (code, set) in instructions.iter().rev().zip(liveset) {
        match code {
            Op1(op, box e) if is_arithmetic(op.as_str()) => {
                for &v in set.iter() {
                    if v == e { continue; }
                    graph.add_edge(v, e);
                }
            },
            Op2(op, box e1, box e2) if is_arithmetic(op.as_str()) => {
                for r in saved_register.iter() {
                    for &v in set.iter() {
                        graph.add_edge(r, v);
                    }
                }
            },
            Op2(op, box e1, box e2) if op.as_str() == "movq" => {
                for &v in set.iter() {
                    if v == e2 || v == e1 { continue; }
                    graph.add_edge(e2, v);
                }
            },
            Callq(label) => {

            },
            _ => (),
        }
    }
    Graph::new()
}

fn is_arithmetic(op: &str) -> bool {
    op == "negq" || op == "addq" || op == "subq" || op == "idivq"
}
// -----------------

// ----------------- assign homes -----------------------------------
const FRAME: usize = 16;
const BYTE: usize = 8;
pub fn assign_homes(block: x86Block) -> x86Block {
    let x86Block { locals, instructions, stack_space, name} = block;
    let stack_space = align_address(locals.len() * BYTE, FRAME);
    let symtable = build_symbol_table(&locals);
    let instructions = assign_homes_helper(instructions, &symtable);
    return x86Block {locals, instructions, stack_space, name};
}

#[inline]
fn align_address(space: usize, align: usize) -> usize {
    let remain = space % align;
    if remain == 0 { space } else { space + align - remain }
}

fn build_symbol_table(locals: &Vec<x86>) -> SymTable<&x86, x86> {
    use x86::*;
    let mut symtable = SymTable::new();
    for (i, var) in locals.iter().enumerate() {
        symtable.bind(var, Deref(Box::new(RBP), (i+1) as i64 * -8));
    }
    return symtable;
}

fn assign_homes_helper(instr: Vec<x86>, symtable: &SymTable<&x86, x86>) -> Vec<x86> {
    use x86::*;
    fn helper(expr: x86, symtable: &SymTable<&x86, x86>) -> x86 {
        match expr {
            Var(x)  => symtable.map.get(&Var(x.to_string())).unwrap().clone(),
            Op1(op, box e) => Op1(op.to_string(), Box::new(helper(e, symtable))),
            Op2(op, box e1, box e2) => Op2(op.to_string(), Box::new(helper(e1, symtable)), Box::new(helper(e2, symtable))),
            e => e,
        }
    }
    let instr = instr.into_iter().map(|e| helper(e, symtable)).collect();
    return instr;
}

// ---------------------------------- patch instructions ---------------------------------------------
pub fn patch_instructions(block: x86Block) -> x86Block {
    use x86::*;
    let x86Block { locals, instructions, stack_space, name } = block;
    let mut new_instructions = vec![];
    for instr in instructions.into_iter() {
        match instr {
            Op2(movq, box Deref(box reg1, n1), box Deref(box reg2, n2)) => {
                new_instructions.push( Op2(movq.clone(), Box::new(Deref(Box::new(reg1), n1)), Box::new(RAX)));
                new_instructions.push( Op2(movq, Box::new(RAX), Box::new(Deref(Box::new(reg2), n2))));
            },
            e => new_instructions.push( e ),
        }
    }
    return x86Block { locals, instructions: new_instructions, stack_space, name };
}

// ---------------------------------- print x86 ---------------------------------------------
use std::io::Write;
use std::fs::File;
pub fn print_x86(block: x86Block, filename: &str) -> std::io::Result<()> {
    let prelude = build_prelude(block.stack_space, &block.name);
    let conclusion = build_conclusion(block.stack_space);

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

