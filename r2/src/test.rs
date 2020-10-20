use std::collections::HashSet;
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
    let env = Rc::new(SymTable::new());
    let map = hashmap!(string!("Jenny") => 100, string!("Good") => 42);
    let env2 = SymTable::<String, i64>::extend(map, &env);
    let v = env2.lookup(&"Jenny".to_string());
    assert_eq!(*v, 100);
}
#[test]
fn test_let() {
    let exp = Let(Box::new(Var(string!("x"))), Box::new(Int(8)), Box::new(Prim2(string!("+"), Box::new(Var(string!("x"))), Box::new(Int(34)))));
    assert_eq!(Int(42), interp_r2(exp));
}
#[test]
fn test_nest_let() {
    let exp = Let(Box::new(Var(string!("x"))), Box::new(Int(8)), 
                Box::new(Let(Box::new(Var(string!("y"))), Box::new(Int(34)), Box::new(Prim2(string!("+"), Box::new(Var(string!("x"))), Box::new(Var(string!("y"))))))));
    assert_eq!(Int(42), interp_r2(exp));
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
fn test_interp_r2() {
    let e = "(let (x 8) (let (y 34) (+ x y)))";
    let exp = parse(e);
    let res = interp_r2(exp);
    assert_eq!(res, Int(42));
}
#[test]
fn test_interp_r2_2() {
    let e = "(let (x (let (y 10) 
                        (- y))) 
                (let (z (+ 1 2))
                    (+ x z)))";
    let exp = parse(e);
    let res = interp_r2(exp);
    assert_eq!(Int(-7), res);
}
#[test]
fn test_interp_r2_shadow() {
    let e = "(let (x 10)
                (let (y 4)
                    (let (x 20)
                        (+ x y))))";
    let exp = parse(e);
    let res = interp_r2(exp);
    assert_eq!(Int(24), res);
}
#[test]
fn test_interp_r2_shadow_2() {
    let e = "(let (x (let (x 4) 
                        (+ x 1)))
                (+ x 2))";
    let exp = parse(e);
    let res = interp_r2(exp);
    assert_eq!(res, Int(7));
}
#[test]
fn test_interp_r2_minus() {
    let e = "(let (x 10)
                (- x 2))";
    let exp = parse(e);
    let res = interp_r2(exp);
    assert_eq!(res, Int(8));
}
#[test]
fn test_interp_r2_cmp() {
    let e = "(let (x 10)
                (> x 2))";
    let exp = parse(e);
    let res = interp_r2(exp);
    assert_eq!(res, Bool(true));
}
#[test]
fn test_interp_r2_eq() {
    let e = "(let (x 10)
                (eq? x 2))";
    let exp = parse(e);
    let res = interp_r2(exp);
    assert_eq!(res, Bool(false));
}
#[test]
fn test_interp_r2_logical() {
    let e = "(let (x #f)
                (let (y #t)
                    (or x y)))";
    let exp = parse(e);
    let res = interp_r2(exp);
    assert_eq!(res, Bool(true));
}
#[test]
fn test_interp_r2_logical_2() {
    let e = "(let (x #f)
                (let (y #t)
                    (not x)))";
    let exp = parse(e);
    let res = interp_r2(exp);
    assert_eq!(res, Bool(true));
}
#[test]
fn test_interp_r2_if() {
    let e = "(if #t #f #t)";
    let exp = parse(e);
    let res = interp_r2(exp);
    assert_eq!(res, Bool(false));
}
#[test]
fn test_interp_r2_if2() {
    let e = "(if (not (if (and (let (x 10)
                            (let (y 32)
                                (eq? (+ x y) 42)))
                            #f)
                        #f #t))
                24 42)";
    let exp = parse(e);
    let res = interp_r2(exp);
    assert_eq!(res, Int(42));
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
    let res = interp_r2(exp);
    assert_eq!(res, Int(7));
}
#[test]
fn test_remove_complex_opera() {
    let e = "(+ 10 (- 8))";
    let exp = parse(e);
    let exp = remove_complex_opera(exp);
    if let Let(box Var(x), box Prim1(_sub, box Int(n)), box Prim2(_add, box Int(n1), box Var(x1))) = &exp {
        assert_eq!(x, x1);
        assert_eq!(n, &8);
        assert_eq!(n1, &10);
    } else {
        panic!("remove_complex_opera fails!");
    }
    assert_eq!(interp_r2(exp), Int(2));
}
#[test]
fn test_remove_complex_opera2() {
    let e = "(+ (+ 10 32) (+ 39 3))";
    let exp = parse(e);
    let exp = remove_complex_opera(exp);
    if let Let(box Var(x), box Prim2(_add, box Int(n10), box Int(n32)), 
        box Let(box Var(y), box Prim2(add, box Int(n39), box Int(n3)), 
            box Prim2(add_, box Var(x_), box Var(y_)))) = &exp {
        assert_eq!(x, x_);
        assert_eq!(y, y_);
        assert_eq!(n3, &3);
    } else {
        panic!("remove_complex_opera fails!");
    }
    assert_eq!(interp_r2(exp), Int(84));
}
use crate::syntax::{C0Program, C0};
#[test]
fn test_explicate_control() {
    use C0::*;
    let e = "(let (x (+ 1 2))
                x)";
    let exp = parse(e);
    let exp = explicate_control(exp);
    let C0Program { locals, cfg: mut blocks } = exp;
    let (label, codes) = blocks.pop().unwrap();
    assert!(matches!(&codes, Seq(box Assign(box Var(x), box Prim2(add, box Int(n1), box Int(n2))), box Return(box Var(x_))) if x == x_));
}

use crate::syntax::{x86, x86Program};
#[test]
fn test_select_instruction() {
    use x86::*;
    let e = "(let (a 42)
                (let (b a)
                    b))";
    let exp = parse(e);
    let exp = remove_complex_opera(exp);
    let exp = explicate_control(exp);
    let prog = select_instruction(exp);
    let x86Program { locals, mut cfg, stack_space } = prog;
    let instructions = cfg.get(&"start".to_string()).unwrap();
    match instructions.as_slice() {
        [_mov1, _mov2, _mov3, jump] => assert!(matches!(jump, Jmp(label) if label.as_str() == "conclusion")),
        _ => panic!("fails in select instruction"),
    }
}
#[test]
fn test_allocate_registers() {
    use x86::*;
    let e = "(let (a 42)
                (let (b a)
                    b))";
    let exp = parse(e);
    let exp = remove_complex_opera(exp);
    let exp = explicate_control(exp);
    let block = select_instruction(exp);
    let block = allocate_registers(block);
    let x86Program { locals, cfg, stack_space } = block;
    let instructions = cfg.get(&"start".to_string()).unwrap();
    match instructions.as_slice() {
        [_mov1, mov2, _mov3, _jump] => {
            assert!(matches!(mov2, Op2(mov, box reg1, box reg2) 
                                    if mov.as_str() == "movq" && reg1 == reg2));
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
    let exp = parse(e);
    let exp = remove_complex_opera(exp);
    let exp = explicate_control(exp);
    let block = select_instruction(exp);
    let block = allocate_registers(block);
    let block = patch_instructions(block);
    let x86Program { locals, cfg, stack_space } = block;
    let instructions = cfg.get(&"start".to_string()).unwrap();
    match instructions.as_slice() {
        [_mov1, mov2, _jump] => {
            assert!(matches!(mov2, Op2(mov, box rdx, box r) 
                                    if mov.as_str() == "movq" && rdx == &x86::RDX));
        },
        _ => panic!("test patch_instructions fails"),
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
    let jne = x86::Jmpif("ne".to_string(), "somewhere".to_string());
    assert_eq!(format!("{}", jne), "jne somewhere");
}
#[test]
#[should_panic]
fn test_invalid_x86_display() {
    let var = x86::Var("haha".to_string());
    let f = format!("{}", var);
}
#[test]
fn test_uncover_live() {
    use x86::*;
    let exp = "(let (a 5)
               (let (b 30)
               (let (c a)
               (let (b 10)
               (let (c (+ b c))
                   c)))))";
    let exp = parse(exp);
    let exp = remove_complex_opera(exp);
    let exp = explicate_control(exp);
    let block = select_instruction(exp);
    let x86Program {mut cfg, stack_space, locals } = block;
    let instructions = cfg.get(&"start".to_string()).unwrap();
    let liveset = uncover_live(instructions, HashSet::new());
    let a = Var("a".to_string());
    let b = Var("b".to_string());
    let c = Var("c".to_string());
    let expect = vec![
        hashset!(&a), hashset!(&a), hashset!(&c), hashset!(&c, &b),
        hashset!(&c), hashset!(), hashset!(), 
    ];
    assert_eq!(expect, liveset);
}
#[test]
fn test_uncover_live2() {
    use x86::*;
    let exp = "(let (a 5)
               (let (b 30)
               (let (c a)
               (let (b 10)
               (let (c (+ b c))
                   c)))))";
    let exp = parse(exp);
    let exp = remove_complex_opera(exp);
    let exp = explicate_control(exp);
    let block = select_instruction(exp);
    let liveset = uncover_live_prog(&block);
    let a = Var("a".to_string());
    let b = Var("b".to_string());
    let c = Var("c".to_string());
    let expect = vec![
        hashset!(&a), hashset!(&a), hashset!(&c), hashset!(&c, &b),
        hashset!(&c), hashset!(), hashset!(), 
    ];
    assert_eq!(Some(&expect), liveset.get(&"start".to_string()));
}


use crate::typesystem::*;
use crate::typesystem::RType::*;
#[test]
fn test_type_checker() {
    let exp = "(let (x 10) 
                   (+ x 29))";
    let exp = parse(exp);
    let etype = type_checker(&exp);
    assert_eq!(etype, Integer);
    let exp = "(- 10 20)";
    let exp = parse(exp);
    let etype = type_checker(&exp);
    assert_eq!(Integer, etype);
}
#[test]
#[should_panic]
fn test_type_checker2() {
    let exp = "(if (not 1) #t #f)";
    let exp = parse(exp);
    let etype = type_checker(&exp);
}
#[test]
fn test_type_checker3() {
    let exp = "(and (> 10 20) (eq? 10 42))";
    let exp = parse(exp);
    let etype = type_checker(&exp);
    assert_eq!(etype, Boolean);
}
#[test]
fn test_shrink2() {
    fn helper(e: &str, expect: Expr) {
        let exp = parse(e);
        let exp = shrink(exp);
        let res = interp_r2(exp);
        assert_eq!(res, expect);
    }
    helper("(>= 3 3)", Bool(true));
    helper("(or #f #t", Bool(true));
    helper("(and #t #t", Bool(true));
    helper("(- 10 30)", Int(-20));
    helper("(<= 4 3)", Bool(false));
    helper("(and (> 10 20) (eq? 10 42))", Bool(false));
}
#[test]
fn test_shrink3() {
    let s = "(- 10 20)";
    let e = parse(s);
    let e = shrink(e);
    matches!(e, Prim2(op, box Int(n10), box Prim1(op2, box Int(n20))) if n10 == 10 && op2.as_str() == "-");
}