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
    match expr {
        Seq(box Assign(box x, box e), box tail) => {
            let e = flatten_seq_helper(e, stack);
            let assign = Assign(Box::new(x), Box::new(e));
            stack.push(assign);
            return flatten_seq_helper(tail, stack);
        }
        e => e,
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
            Var(x)  => symtable.map.get(&Var(x)).unwrap().clone(),
            Op1(op, box e) => Op1(op, Box::new(helper(e, symtable))),
            Op2(op, box e1, box e2) => Op2(op, Box::new(helper(e1, symtable)), Box::new(helper(e2, symtable))),
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