use crate::*;
use crate::semantic::Environment;
use crate::{hashmap, string};
use crate::compiler::*;
#[test]
fn test_env() {
    let mut env = Environment::new();
    let (var, val) = (string!("mory"), 100);
    env.bind(var, val);
    let v = env.lookup(&"mory".to_string());
    assert_eq!(*v, 100);
}
#[test]
fn test_env2() {
    let mut env = Environment::new();
    let (var, val) = (string!("mory"), 100);
    env.bind(var, val);
    let map = hashmap!(string!("Jenny") => 100, string!("Good") => 42);
    let env2 = env.extend(map);
    let v = env2.lookup(&"Jenny".to_string());
    assert_eq!(*v, 100);
}
#[test]
fn test_let() {
    let exp = Let(Box::new(Var(string!("x"))), Box::new(Int(8)), Box::new(Prim(string!("+"), Box::new([Var(string!("x")), Int(34)]))));
    assert_eq!(42, interp_r1(exp));
}
#[test]
fn test_nest_let() {
    let exp = Let(Box::new(Var(string!("x"))), Box::new(Int(8)), 
                Box::new(Let(Box::new(Var(string!("y"))), Box::new(Int(34)), Box::new(Prim(string!("+"), Box::new([Var(string!("x")), Var(string!("y"))]))))));
    assert_eq!(42, interp_r1(exp));
}
#[test]
fn test_parser() {
    let e = "(let (x 8) (let (y 34) (+ x y)))";
    let exp = Let(Box::new(Var(string!("x"))), Box::new(Int(8)), 
                Box::new(Let(Box::new(Var(string!("y"))), Box::new(Int(34)), Box::new(Prim(string!("+"), Box::new([Var(string!("x")), Var(string!("y"))]))))));
    let res = parse(e);
    assert_eq!(exp, res);
}
#[test]
fn test_parser_read() {
    let e = "(read)";
    let exp = Prim ( string!("read"), Box::new([]));
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
    if let Let(box Var(x1), box Let(box Var(x2), _i1, box Prim(_add, box [Var(x2_), _i2])), box Prim(_add1, box [Var(x1_), _i3])) = &exp {
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
    if let Let(box Var(x), box Prim(_sub, box [Int(n)]), box Prim(_add, box [Int(n1), Var(x1)])) = &exp {
        assert_eq!(x, x1);
        assert_eq!(n, &8);
        assert_eq!(n1, &10);
    } else {
        panic!("remove_complex_opera fails!");
    }
    assert_eq!(interp_r1(exp), 2);
}
use crate::semantic::{C0Program, C0};
#[test]
fn test_explicate_control() {
    use C0::*;
    let e = "(let (x (+ 1 2))
                x)";
    let mut exp = parse(e);
    let exp = explicate_control(&mut exp);
    if let C0Program { locals, cfg: mut blocks } = exp {
        let (label, codes) = blocks.pop().unwrap();
        assert!(matches!(&codes, Seq(box Assign(box Var(x), box Prim(add, box [Int(n1), Int(n2)])), box Return(box Var(x_))) if x == x_));
    } else {
        panic!("explicate_control fails in C0Program expand");
    }
}
use crate::semantic::{x86, x86Block, x86Program};
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
    let x86Block { locals, instr, stack_space } = block;
    match instr.as_slice() {
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
    let x86Block { locals, instr, stack_space } = block;
    match instr.as_slice() {
        [mov1, mov2, mov3, _jump] => {
            assert!(matches!(mov1, Instr(mov, box [Imm(n), Deref(box reg, disp)]) 
                                    if mov.as_str() == "movq" && *n == 42 && *reg == x86::RBP && *disp == -8));
            assert!(matches!(mov2, Instr(mov, box [Deref(box reg1, disp1), Deref(box reg2, disp2)]) 
                                    if mov.as_str() == "movq" && reg1 == reg2 && *disp1 == -8 && *disp2 == -16));
            assert!(matches!(mov3, Instr(mov, box [Deref(box reg, disp), rax]) 
                                    if mov.as_str() == "movq" && *reg == x86::RBP && *disp == -16 && *rax == x86::RAX));
        },
        _ => panic!("test assign_home fails"),
    }
}