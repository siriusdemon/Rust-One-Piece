use crate::*;
use crate::syntax::SymTable;
use crate::{hashmap, string};
use crate::compiler::*;
use crate::parser::parse;

#[test]
fn test_env() {
    let mut env = SymTable::new();
    let (var, val) = (string!("mory"), 100);
    env.bind(var, val);
    let v = env.lookup(&"mory".to_string());
    assert_eq!(*v, 100);
}
#[test]
fn test_env2() {
    let mut env = Rc::new(SymTable::new());
    let map = hashmap!(string!("Jenny") => 100, string!("Good") => 42);
    let env2 = SymTable::<String, i64>::extend(map, &env);
    let v = env2.lookup(&"Jenny".to_string());
    assert_eq!(*v, 100);
}
#[test]
fn test_let() {
    let exp = Let(Box::new(Var(string!("x"))), Box::new(Int(8)), Box::new(Prim2(string!("+"), Box::new(Var(string!("x"))), Box::new(Int(34)))));
    assert_eq!(42, interp_r1(exp));
}
#[test]
fn test_nest_let() {
    let exp = Let(Box::new(Var(string!("x"))), Box::new(Int(8)), 
                Box::new(Let(Box::new(Var(string!("y"))), Box::new(Int(34)), Box::new(Prim2(string!("+"), Box::new(Var(string!("x"))), Box::new(Var(string!("y"))))))));
    assert_eq!(42, interp_r1(exp));
}
#[test]
fn test_parser() {
    let e = "(let (x 8) (let (y 34) (+ x y)))";
    let exp = Let(Box::new(Var(string!("x"))), Box::new(Int(8)), 
                Box::new(Let(Box::new(Var(string!("y"))), Box::new(Int(34)), Box::new(Prim2(string!("+"), Box::new(Var(string!("x"))), Box::new(Var(string!("y"))))))));
    let res = parse(e);
    assert_eq!(exp, res);
}
#[test]
fn test_parser_read() {
    let e = "(read)";
    let exp = Prim0 ( string!("read"));
    let res = parse(e);
    assert_eq!(exp, res);
}
#[test]
fn test_parser_atom() {
    let e = "1";
    let exp = Int(1);
    let res = parse(e);
    assert_eq!(exp, res);
}
#[test]
fn test_interp_r1() {
    let e = "(let (x 8) (let (y 34) (+ x y)))";
    let exp = parse(e);
    let res = interp_r1(exp);
    assert_eq!(res, 42);
}
#[test]
fn test_interp_r1_2() {
    let e = "(let (x (let (y 10) 
                        (- y))) 
                (let (z (+ 1 2))
                    (+ x z)))";
    let exp = parse(e);
    let res = interp_r1(exp);
    assert_eq!(-7, res);
}
#[test]
fn test_interp_r1_shadow() {
    let e = "(let (x 10)
                (let (y 4)
                    (let (x 20)
                        (+ x y))))";
    let exp = parse(e);
    let res = interp_r1(exp);
    assert_eq!(24, res);
}
#[test]
fn test_interp_r1_shadow_2() {
    let e = "(let (x (let (x 4) 
                        (+ x 1)))
                (+ x 2))";
    let exp = parse(e);
    let res = interp_r1(exp);
    assert_eq!(res, 7);
}
#[test]
#[should_panic(expected="variable could not be digit!")]
fn test_bad_variable_name() {
    let e = "(let (19 20) (+ 19 19))";
    let _exp = parse(e);
}
#[test]
fn test_uniquify() {
    let e = "(let (x (let (x 4) 
                    (+ x 1)))
            (+ x 2))";
    let exp = parse(e);
    let exp = uniquify(exp);
    if let Let(box Var(x1), box Let(box Var(x2), _i1, box Prim2(_add, box Var(x2_), box _i2)), box Prim2(_add1, box Var(x1_), box _i3)) = &exp {
        assert_eq!(x1, x1_);
        assert_eq!(x2, x2_);
        assert_ne!(x1, x2);
    } else {
        panic!("uniquify fails!");
    }
    let res = interp_r1(exp);
    assert_eq!(res, 7);
}
#[test]
fn test_remove_complex_opera() {
    let e = "(+ 10 (- 8))";
    let mut exp = parse(e);
    let exp = remove_complex_opera(&mut exp);
    if let Let(box Var(x), box Prim1(_sub, box Int(n)), box Prim2(_add, box Int(n1), box Var(x1))) = &exp {
        assert_eq!(x, x1);
        assert_eq!(n, &8);
        assert_eq!(n1, &10);
    } else {
        panic!("remove_complex_opera fails!");
    }
    assert_eq!(interp_r1(exp), 2);
}
use crate::syntax::{C0Program, C0};
#[test]
fn test_explicate_control() {
    use C0::*;
    let e = "(let (x (+ 1 2))
                x)";
    let mut exp = parse(e);
    let exp = explicate_control(&mut exp);
    if let C0Program { locals, cfg: mut blocks } = exp {
        let (label, codes) = blocks.pop().unwrap();
        assert!(matches!(&codes, Seq(box Assign(box Var(x), box Prim2(add, box Int(n1), box Int(n2))), box Return(box Var(x_))) if x == x_));
    } else {
        panic!("explicate_control fails in C0Program expand");
    }
}
use crate::syntax::{x86, x86Block, x86Program};
#[test]
fn test_select_instruction() {
    use x86::*;
    let e = "(let (a 42)
                (let (b a)
                    b))";
    let mut exp = parse(e);
    let mut exp = remove_complex_opera(&mut exp);
    let exp = explicate_control(&mut exp);
    let block = select_instruction(exp);
    let x86Block { locals, instructions, stack_space, name } = block;
    match instructions.as_slice() {
        [_mov1, _mov2, _mov3, jump] => assert!(matches!(jump, Jmp(label) if label.as_str() == "conclusion")),
        _ => panic!("fails in select instruction"),
    }
}
#[test]
fn test_assign_homes() {
    use x86::*;
    let e = "(let (a 42)
                (let (b a)
                    b))";
    let mut exp = parse(e);
    let mut exp = remove_complex_opera(&mut exp);
    let exp = explicate_control(&mut exp);
    let block = select_instruction(exp);
    let block = assign_homes(block);
    let x86Block { locals, instructions, stack_space, name } = block;
    match instructions.as_slice() {
        [mov1, mov2, mov3, _jump] => {
            assert!(matches!(mov1, Op2(mov, box Imm(n), box Deref(box reg, disp)) 
                                    if mov.as_str() == "movq" && *n == 42 && *reg == x86::RBP && *disp == -8));
            assert!(matches!(mov2, Op2(mov, box Deref(box reg1, disp1), box Deref(box reg2, disp2)) 
                                    if mov.as_str() == "movq" && reg1 == reg2 && *disp1 == -8 && *disp2 == -16));
            assert!(matches!(mov3, Op2(mov, box Deref(box reg, disp), box rax) 
                                    if mov.as_str() == "movq" && *reg == x86::RBP && *disp == -16 && *rax == x86::RAX));
        },
        _ => panic!("test assign_home fails"),
    }
}

#[test]
fn test_patch_instructions() {
    use x86::*;
    let e = "(let (a 42)
                (let (b a)
                    b))";
    let mut exp = parse(e);
    let mut exp = remove_complex_opera(&mut exp);
    let exp = explicate_control(&mut exp);
    let block = select_instruction(exp);
    let block = assign_homes(block);
    let block = patch_instructions(block);
    let x86Block { locals, instructions, stack_space, name } = block;
    match instructions.as_slice() {
        [_mov1, mov2, mov3, _mov4, _jump] => {
            assert!(matches!(mov2, Op2(mov, box Deref(box reg1, disp1), box rax) 
                                    if mov.as_str() == "movq"  && *rax == x86::RAX));
            assert!(matches!(mov3, Op2(mov, box rax, box Deref(box reg, disp)) 
                                    if mov.as_str() == "movq" && *reg == x86::RBP && *disp == -16 && *rax == x86::RAX));
        },
        _ => panic!("test assign_home fails"),
    }
}

#[test]
fn test_x86_display() {
    let n = x86::Imm(10);
    assert_eq!(format!("{}", n), "$10");
    let deref = x86::Deref(Box::new(x86::RBP), 10);
    assert_eq!(format!("{}", deref), "10(%rbp)");
    let instr = x86::Op2("movq".to_string(), Box::new(n), Box::new(deref));
    assert_eq!(format!("{}", instr), "movq $10, 10(%rbp)");

    let call = x86::Callq("read_int".to_string());
    let push = x86::Pushq(Box::new(x86::RBP));
    let pop = x86::Popq(Box::new(x86::RSP));
    assert_eq!(format!("{}", call), "callq read_int");
    assert_eq!(format!("{}", push), "pushq %rbp");
    assert_eq!(format!("{}", pop), "popq %rsp");
}
#[test]
#[should_panic]
fn test_invalid_x86_display() {
    let var = x86::Var("haha".to_string());
    let f = format!("{}", var);
}