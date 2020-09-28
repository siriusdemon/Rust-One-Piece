use crate::parser::{List, Atom, scan, parse};
use crate::*;

#[test]
fn test_r0() {
    let p3 = Program {
        expr: Prim ("+".to_string(), Box::new([ Int(10), Int(32) ]))
    };
    let r = interp_r0(&p3);
    assert_eq!(r, 42);
}
#[test]
fn test_scan() {
    let s = "(1 2 (+ 1 2))";
    let expr = scan(s);
    let t = List(vec![Atom("1".to_string()), Atom("2".to_string()), 
                        List(vec![Atom("+".to_string()), Atom("1".to_string()), Atom("2".to_string())])]);
    assert_eq!(expr, t);
}
#[test]
fn test_parse() {
    let s = "(+ 1 2)";
    let expr = Prim ("+".to_string(), Box::new([Int(1), Int(2)]));
    assert_eq!(parse(s), expr);
    let s = "(- 10)";
    let expr = Prim ("-".to_string(), Box::new([Int(10)]));
    assert_eq!(parse(s), expr);
    let s = "(read)";
    let expr = Prim ("read".to_string(), Box::new([]));
    assert_eq!(parse(s), expr);
}
#[test]
fn test_interp() {
    let s = "(+ 1 2)";
    let expr = parse(s);
    let r = interp_r0(&Program{expr});
    assert_eq!(r, 3);
}
