from py2 import Scanner
from py2 import empty_env, lookup, extend, UnknownVar
from py2 import parse_expr
from py2 import PyDefvar, PyInt, PyVar, PyDefun, PyCall, PyOp2, PyNull

def helper(s, expected):
    scan = Scanner(s)
    for tok in expected:
        assert(tok == scan.next_token()[0])


def test_scanner1():
    s = "y = (x + 1)"
    scan = Scanner(s)
    assert("y" == scan.next_token()[0])
    assert("=" == scan.next_token()[0])
    assert("(" == scan.next_token()[0])
    assert("x" == scan.next_token()[0])
    assert("+" == scan.next_token()[0])
    assert("1" == scan.next_token()[0])
    assert(")" == scan.next_token()[0])

def test_scanner2():
    s = "y = lambda x: x + 1\n"
    scan = Scanner(s)
    assert("y" == scan.next_token()[0])
    assert("=" == scan.next_token()[0])
    assert("lambda" == scan.next_token()[0])
    assert("x" == scan.next_token()[0])
    assert(":" == scan.next_token()[0])
    assert("x" == scan.next_token()[0])
    assert("+" == scan.next_token()[0])
    assert("1" == scan.next_token()[0])
    assert("\n" == scan.next_token()[0])

def test_scanner3():
    s = "x=10\n  y=20  "
    scan = Scanner(s)
    assert("x" == scan.next_token()[0])
    assert("=" == scan.next_token()[0])
    assert("10" == scan.next_token()[0])
    assert("\n" == scan.next_token()[0])
    assert("  " == scan.next_token()[0])
    assert("y" == scan.next_token()[0])
    assert("=" == scan.next_token()[0])
    assert("20" == scan.next_token()[0])
    assert(None == scan.next_token()[0])

def test_scanner4():
    # function
    s = "def a(b, c): return b + c"
    helper(s, ["def", "a", "(", "b", ',', "c", ")", ":", 
               "return", "b", "+", "c", None]) 

def test_scanner5():
    s = "f(a, 10)"
    helper(s, ['f', '(', 'a', ',', '10', ')'])

def test_env():
    env = empty_env()
    env = extend("x", 10, env)
    env = extend("y", 20, env)
    assert(lookup(env, "x") == 10)
    assert(lookup(env, "y") == 20)


def test_env2():
    env = empty_env()
    env1 = extend("x", 10, env)
    env2 = extend("x", 20, env1)
    assert(lookup(env1, "x") == 10)
    assert(lookup(env2, "x") == 20)

def test_env3():
    env = empty_env()
    try:
        lookup(env, "hah") 
    except UnknownVar as e:
        print(e)
    else:
        raise Exception("Fail!") 


def test_parse1():
    s = "x = 10"
    scan = Scanner(s)
    scan.next_token()
    expr = parse_expr(scan)
    assert isinstance(expr, PyDefvar)
    assert expr.var == PyVar('x')
    assert expr.val == PyInt(10)

def test_parse2():
    s = "f(10, x)"
    scan = Scanner(s)
    scan.next_token()
    expr = parse_expr(scan)
    assert isinstance(expr, PyCall)
    assert expr.fn == PyVar('f')
    assert expr.args == [PyInt(10), PyVar('x')]


    