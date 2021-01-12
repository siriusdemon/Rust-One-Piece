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
    // e will be (let (x (- 8)
    //              (+ 10 x)))
    let exp = parse(e);
    let exp = type_checker(exp);
    let mut locals = hashmap!();
    let exp = expose_allocation(exp, &mut locals);
    let exp = remove_complex_opera(exp);
    if let Hastype(box let_, t) = exp {
        assert_eq!(t, Integer);
        if let Let(box x, box e, box body) = let_ {
            assert!(matches!(e, Hastype(box Prim1(neg, box Hastype(box i, itype)), t) 
                if itype == Integer && neg.as_str() == "-" && t == Integer && i == Int(8)));
        } else {
            panic!("Failed!");
        }
    } else {
        panic!("Failed!");
    }
}
#[test]
fn test_remove_complex_opera2() {
    let e = "(+ (+ 10 32) (+ 39 3))";
    // e = (let (x (+ 10 32))
    //     (let (y (+ 39 3))
    //        (+ x y)))
    let exp = parse(e);
    let exp = type_checker(exp);
    let mut locals = hashmap!();
    let exp = expose_allocation(exp, &mut locals);
    let exp = remove_complex_opera(exp);
    println!("{}", exp);
    assert!(matches!(exp, 
        Hastype(box 
            Let(box x, box plus1, box Hastype(box 
                Let(box y, box plus2, box ret),
                t2)
            ),
            t1)
        if t1 == Integer && t2 == Integer
    ));
}

use crate::syntax::{C0Program, C0};
#[test]
fn test_explicate_control() {
    use C0::*;
    let e = "(let (x (+ 1 2))
                x)";
    let exp = parse(e);
    let exp = type_checker(exp);
    let exp = explicate_control(exp);
    let C0Program { locals, cfg: mut blocks } = exp;
    let codes = blocks.get("start").unwrap();
    assert!(matches!(&codes, Seq(box Assign(box Var(x), box Prim2(add, box Int(n1), box Int(n2))), box Return(box Var(x_))) if x == x_));
}
#[test]
fn test_explicate_control2() {
    // just make sure it pass
    use C0::*;
    // let e = "(let (x (vector 42 (vector 10)))
    //          (let (y (vector-ref x 1))
    //             (vector-set! y 0 #t)))";
    // let e = "(let (x 1)
    //          (let (y 2)
    //            (+ x y)))";
    let e = "(let (x (vector 42))
                (vector-ref x 0))";
    let exp = parse(e);
    let exp = type_checker(exp);
    let exp = shrink(exp);
    let mut locals = hashmap!();
    let exp = expose_allocation(exp, &mut locals);
    let exp = remove_complex_opera(exp);
    println!("{}", exp);
    let _exp = explicate_control(exp);
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
    let exp = type_checker(exp);
    assert!(matches!(exp, Hastype(box _e, t) if t == Integer));
    let exp = "(- 10 20)";
    let exp = parse(exp);
    let exp = type_checker(exp);
    assert!(matches!(exp, Hastype(box _e, t) if t == Integer));
}
#[test]
#[should_panic]
fn test_type_checker2() {
    let exp = "(if (not 1) #t #f)";
    let exp = parse(exp);
    let _exp = type_checker(exp);
    println!("{:?}", _exp);
}
#[test]
fn test_type_checker3() {
    let exp = "(and (> 10 20) (eq? 10 42))";
    let exp = parse(exp);
    let exp = type_checker(exp);
    assert!(matches!(exp, Hastype(box _e, t) if t == Boolean));
}
#[test]
fn test_vector_type() {
    let e = "(let (c (vector 1 2))
                c)";
    let e = parse(e);
    let e = type_checker(e);
    assert!(matches!(e, Hastype(box _e, t) if t == RType::Vector));
}
#[test]
fn test_void_type() {
    let e = "(let (c (vector 1 2))
                (vector-set! c 0 42))";
    let e = parse(e);
    let e = type_checker(e);
    assert!(matches!(e, Hastype(box _e, t) if t == RType::Void));
}
#[test]
fn test_shrink3() {
    let s = "(- 10 20)";
    let e = parse(s);
    let e = type_checker(e);
    let e = shrink(e);
    assert!(matches!(e, 
        Hastype(box Prim2(op, box Hastype(box Int(n10), Integer), box 
            Hastype(box Prim1(op2, box Hastype(box Int(n20), Integer)), Integer)), Integer)
        if n10 == 10 && op2.as_str() == "-"));
}
#[test]
fn test_shrink4() {
    let e = "(if #f (- 10 20) 42)";
    let e = parse(e);
    let e = type_checker(e);
    let e = shrink(e);
    println!("{:?}", e);
    assert!(matches!(e, 
        Hastype(box If(box Hastype(box Bool(false), Boolean), box
            Hastype(box Prim2(op, box Hastype(box Int(n10), Integer), box 
                Hastype(box Prim1(op2, box Hastype(box Int(n20), Integer)), Integer)), Integer), box
            Hastype(box Int(42), Integer)), Integer)));
}
#[test]
fn test_optimize_jump() {
    let e = "(if (if #f #t #f) #t #f)"; 
    let expr = parse(e);
    let expr = remove_complex_opera(expr);
    let expr = explicate_control(expr);
    assert_eq!(5, expr.cfg.len());
    let expr = optimize_jumps(expr);
    assert_eq!(3, expr.cfg.len());
}
#[test]
fn test_interp_r3() {
    fn helper(e: &str, expect: Expr) {
        let e = parse(e);
        let r = interp_r2(e);
        assert_eq!(r, expect);
    }
    let e = "(let (x (vector 10 #t (vector 42)))
                (vector-ref x 0))";
    helper(e, Int(10));
    let e = "(let (x (vector 10 #t (vector 42)))
                (vector-ref x 1))";
    helper(e, Bool(true));
    let e = "(let (x (vector 10 #t (vector 42)))
                (vector-ref x 2))";
    helper(e, Reference(Rc::new(RefCell::new(Expr::Vector(vec![Int(42)])))));
    let e = "(let (x (vector 10 #t (vector 42)))
                (let (y (vector-set! x 2 #t))
                    y))";
    helper(e, Expr::Void);
    // so, because every time we lookup an variable, we clone it, so,
    // Yes, I know
    let e = "(let (x (vector 10 #t (vector 42)))
                (let (y (vector-set! x 2 #t))
                    x))";
    helper(e, Reference(Rc::new(RefCell::new(Expr::Vector(vec![Int(10), Bool(true), Bool(true)])))));
    let e = "(let (x (vector 10 #t 5))
             (let (z (vector x 1))
             (let (ax (vector-ref z 0))
             (let (void (vector-set! ax 1 #f))
                (vector-ref x 1)))))";
    helper(e, Bool(false));
    let e = "(let (x (vector 24))
             (let (y (vector-set! x 0 40))
                (+ (vector-ref x 0) 2)))";
    helper(e, Int(42));
    let e = "(let (x (vector 42))
             (let (y x)
                (vector-ref y 0)))";
    helper(e, Int(42));
}
#[test]
fn test_unseen_syntax() {
    let e = "(global-value free_ptr)";
    let e = parse(e);
    let ans = _GlobalValue("free_ptr".to_string());
    assert_eq!(ans, e);
    let e = "(collect 1024)";
    let e = parse(e);
    let ans = _Collect(1024);
    assert_eq!(ans, e);
    let e = "(allocate 1024)";
    let e = parse(e);
    let ans = _Allocate(1024, RType::Vector);
    assert_eq!(ans, e);
}
// So, test by eye
// #[test]
// fn test_expose_allocation() {
//     let e = "(vector 42)";
//     let e = parse(e);
//     let e = shrink(e);
//     let e = expose_allocation(e);
//     let answer =  "(let (_tmp 42)
//                    (let (_ (if (< (+ (global-value free_ptr) 16)
//                                   (global-value fromspace_end))
//                              void
//                              (collect 16)))
//                    (let (v (allocate 1))
//                    (let (_ (vector-set! v 0 42))
//                      v))))";
//     let answer = parse(answer);
//     assert_eq!(answer, e);
// }