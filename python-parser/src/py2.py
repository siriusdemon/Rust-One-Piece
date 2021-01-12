import string


def is_num_alpha(c):
    return c in string.ascii_letters or c in string.digits

def is_var(s):
    return s[0] in string.ascii_letters and all(is_num_alpha(c) for c in s)

def all_space(s):
    return all(c==' ' for c in s)

def all_num(s):
    return all(c in string.digits for c in s)
 
# ---------------- Scanner 
# 如果是字母数字，则表示 symbol 或者 number
# 如果是 <= => == = + - * /，则表示分隔
# 如果是空格，则表示分隔或者空格
# 如果是 : ，表示新的块出现了
# scan 阶段，把换行和空格都考虑进入，作为一个 token，由 parse 来决定 indent 是否正确
class Scanner:
    def __init__(self, s):
        self.s = s
        self.tokstream = self.rescan(self.scan(s))
        self.token = None
   
    def next_token(self):
        self.token = next(self.tokstream)
        return self.token
    
    def scan(self, s):
        delimiter = '~+-*/=<>%'
        parse_delimiter = False
        token = ''
        for c in s:
            if parse_delimiter:
                if is_num_alpha(c):
                    # delimiter finish
                    yield token
                    token = c
                    parse_delimiter = False
                elif c == ' ':
                    yield token
                    token = c
                    parse_delimiter = False
                elif c in '()':
                    yield token
                    token = ''
                    yield c
                    parse_delimiter = False
                elif c == ':':
                    yield token
                    token = ''
                    yield c
                    parse_delimiter = False
                elif c == ',':
                    yield token
                    token = ''
                    yield c
                    parse_delimiter = False
                elif c in delimiter:
                    token += c
            else:
                if is_num_alpha(c):
                    if all_space(token) and token != '':
                        yield token
                        token = ''
                    token += c
                elif c == ' ':
                    if all_space(token): # parse space
                        token += c
                    else:
                        yield token
                        token = c
                elif c == '\n':
                    if token != '':
                        yield token
                        token = ''
                    yield c
                elif c in '()':
                    if token:
                        yield token
                        token = ''
                    yield c
                elif c == ':':
                    if token:
                        yield token
                        token = ''
                    yield c
                elif c == ',':
                    if token:
                        yield token
                        token = ''
                    yield c
                elif c in delimiter:
                    parse_delimiter = True
                    yield token
                    token = c
        yield token

    def rescan(self, tokstream):
        "re-scan to remove useless token and generate start and end mark for every token"
        row = 1
        col_s = 1
        col_e = 0
        newline = False
        for token in tokstream:
            if all_space(token):
                if newline:
                    newline = False
                    col_e = col_s + len(token)
                    yield token, [row, col_s, col_e]
                    col_s = col_e
                else:
                    # only update position
                    col_s = col_s + len(token)
            elif token == '\n':
                newline = True
                col_e = col_s + 1
                yield token, [row, col_s, col_e]
                row += 1
                col_s = 1
            else:
                newline = False
                col_e = col_s + len(token)
                yield token, [row, col_s, col_e]
                col_s = col_e
        yield None, [0, 0, 0]

# ----------------- Ast
class PyExpr: 
    def __repr__(self):
        return self.__str__()

class PyNull(PyExpr):
    def __init__(self):
        super().__init__()
    def __str__(self):
        return "PyNull"

class PyInt(PyExpr):
    def __init__(self, val):
        super().__init__()
        self.val = val
    def __str__(self):
        return f"PyInt({self.val})"
    def __eq__(self, other):
        return self.val == other.val
    def __add__(self, other):
        return PyInt(self.val + other.val)

class PyVar(PyExpr):
    def __init__(self, name):
        super().__init__()
        self.name = name
    def __str__(self):
        return f"PyVar({self.name})"
    def __eq__(self, other):
        return other.name == self.name

class PyCall(PyExpr):
    def __init__(self, fn, args):
        super().__init__()
        self.fn = fn
        self.args = args
    def __str__(self):
        return f"PyCall({self.fn})"

class PyDefun(PyExpr):
    def __init__(self, name, args, body, env):
        super().__init__()
        self.name = name
        self.args = args
        self.body = body
        self.env = env
    def __call__(self, params):
        assert(len(params) == len(self.args))
        for (name, val) in zip(self.args, params):
            env = extend(name, val, self.env)
        # function always have a return value
        res = list(interpreter(self.body, env))
        return res[-1]

    def __str__(self):
        return f"PyDefun({self.name})"

class PyOp2(PyExpr):
    def __init__(self, op, e1, e2):
        self.op = op
        self.e1 = e1
        self.e2 = e2

class PyDefvar(PyExpr):
    def __init__(self, var, val):
        super().__init__()
        self.var = var
        self.val = val
    def __str__(self):
        return f"PyDefvar({self.var}, {self.val})"

# ------------- Parser
# We are going to parse a big block
# block contains statements and definition
# definition I mean variables and functions
# statements I mean arithmetic
# a colon means a new block is open
# a return means a function is ended
# there are following type of statement we are going to parse
# 1. variable
# 2. defvar
# 3. defun
# 4. arithmetic
# 5. funcall
# 6. return
# 7. parenthesis
# def parse_defun(): 

# def parse_exprs():

# def parse_parent():

# def parse_funcall(): pass
# def parse_defvar(): 
# def parse_op2():
def expected(scanner, e):
    token, [row, col_s, _] = scanner.token
    assert token == e, f"expect {e} in row {row} column {col_s}"

def match(scanner, e):
    expected(scanner, e)
    scanner.next_token()
    

# def parse_int(scanner):
#     # case
#     #   int
#     token, pos = scanner.token
#     pyint = PyInt(int(token))
#     scanner.next_token()
#     return pyint

# def parse_parent(scanner):
#     # case:
#     #   ( e )
#     scanner.next_token() # eat (
#     e = parse_expr(scanner)
#     expected(scanner, ')')
#     scanner.next_token() # eat )
#     return e

# def parse_ref_call(scanner):
#     # case: 
#     #   x
#     #   f(e)
#     name, pos = scanner.token
#     scanner.next_token()
#     if scanner.token[0] != '(':
#         return PyVar(name)
#     # Go here, it is a funcall 
#     scanner.next_token()
#     args = []
#     if scanner.token[0] != ')':
#         while True:
#             arg = parse_expr(scanner)
#             args.append(arg)
#             if scanner.token[0] == ')': break
#             expected(scanner, ',')
#             scanner.next_token()
#     scanner.next_token()
#     return PyCall(name, args)

# def parse_primary(scanner):
#     token, _ = scanner.token
#     if all_num(token):
#         return parse_int(scanner)
#     elif token == '(':
#         return parse_parent(scanner)
#     else:
#         return parse_ref_call(scanner)

# # Operator Precedence Parsing
# pretable = {
#     '+': 20,
#     '-': 20, 
#     '*': 42,
#     '/': 42,
# }

# def parse_op2(scanner):
#     pass

# def parse_expr(scanner):
#     e1 = parse_primary(scanner)

# here is the mory's version
def parse_expr(scanner, indent=0):
    token, [row, col, _] = scanner.token
    if all_space(token): 
        raise Exception(f"bad indent! at row {row} col {col}")
    elif token == '(':
        return parse_parent(scanner)
    elif token == 'def':
        return parse_defun(scanner, indent)
    elif all_num(token):
        scanner.next_token()
        return PyInt(int(token))
    elif is_var(token):
        sym = token
        token, _ = scanner.next_token()
        if token in '+-*/':
            return parse_op2(sym, scanner)
        elif token == '=':
            scanner.next_token()
            expr = parse_expr(scanner)
            return PyDefvar(PyVar(sym), expr)
        elif token == '(':
            return parse_funcall(sym, scanner)
        elif token == ',' or token == ')': # in a funcall argslist
            return PyVar(sym)
        elif token == '\n':
            return PyVar(sym)
        else:
            raise Exception(f"token go here `{token}`")
    else:
        raise Exception(f"Syntax Error! token {token}")

def parse_parent(scanner):
    scanner.next_token() # eat (
    expr = parse_expr(scanner)
    match(scanner, ')')
    return expr

def parse_op2():
    pass

def parse_defun(scanner, preindent):
    match(scanner, 'def')
    fn, _ = scanner.next_token()
    match(scanner, '(')
    args = []
    if scanner.token[0] != ')':
        # parse args
        assert is_var(scanner.token[0]), "bad variable name"
        arg = PyVar(scanner.token[0])
        args.append(arg)
        scanner.next_token()
        while scanner.token[0] != ')':
            match(scanner, ',')
            arg = PyVar(scanner.token[0])
            args.append(arg)
    match(scanner, ')')
    match(scanner, ':')
    match(scanner, '\n')
    indent = scanner.next_token()
    if len(indent) > preindent:
        # new indent
        body = []
        while True:
            token, _ = scanner.next_token()
            if token == "return":
                scanner.next_token()
                expr = parse_expr(scanner)
            else:
                expr = parse_expr(scanner, len(indent))
            body.append(expr)
            match(scanner, '\n')
            next_indent = scanner.next_token()
            if len(next_indent) == len(indent):
                continue
            elif len(next_indent) > len(indent):
                raise Exception("bad indent!")
            else:
                if preindent == 0 and not all_space(next_indent):
                    # it's back in global and it's a symbol
                    body.append(PyNull)
                    break
                elif preindent == len(next_indent):
                    body.append(PyNull)
                    break
                else:
                    raise Exception("bad indent!")
        return PyDefun(fn, args, body)
    else:
        raise Exception("Bad indent!")
    
    

def parse_funcall(fn, scanner):
    # fn (???)
    match(scanner, '(')
    args = []
    if scanner.token[0] != ')':
        arg = parse_expr(scanner)
        args.append(arg)
        while scanner.token[0] != ')':
            print(scanner.token)
            # parse ,x,y ...
            match(scanner, ',')
            arg = parse_expr(scanner)
            args.append(arg)
    match(scanner, ')')
    return PyCall(PyVar(fn), args)

# def parse_function(codeblock, scanner):
#     name = scanner.next_token()
#     args = []
#     scanner.next_token() # (
#     scanner.next_token() # first args or )
#     while scanner.token != ")":
#         arg = PyVar(scanner.token)
#         args.append(args)
#         scanner.next_token()
#     scanner.next_token() # :
#     # interesting stuff, ignore inline-block right now
#     indent = scanner.next_token()
#     body = CodeBlock(name=name, indent=indent)
#     parse_expr(body, scanner)
#     pyfn = PyFunction(name, args, body)
#     codeblock.fns[name] = pyfn

        

# def parse_assign():
#     pass


# def parse_expr(codeblock: CodeBlock, scanner):
#     token = scanner.next_token()
#     if token == "def":
#         parse_function(codeblock, scanner)
#     else:
#         parse_assign(codeblock, scanner)


# ------------ env
class UnknownVar(Exception): 
    def __init__(self, msg):
        self.msg = msg 
    def __repr__(self):
        return self.msg

def empty_env():
    def search(v: str):
        raise UnknownVar("variable {v} not found!")
    return search

def extend(saved_var, saved_val, env):
    def search(v: str):
        if v == saved_var:
            return saved_val
        else:
            return lookup(env, v)
    return search

def lookup(env, v: str):
    return env(v)


def interpreter_helper(expr, env):
    if isinstance(expr, PyCall):
        fn = lookup(env, expr.fn)
        return fn(expr.args)
    elif isinstance(expr, PyVar):
        return lookup(env, expr)
    elif isinstance(expr, PyInt):
        return expr
    else:
        raise Exception("Other Expr type should be handled by yourself")
    


def interpreter(prog, env) -> PyExpr:
    for expr in prog:
        if isinstance(expr, PyDefvar):
            val = interpreter_helper(expr.val, env)
            env = extend(expr.var, val, env)
        elif isinstance(expr, PyDefun):
            env = extend(expr.name, expr, env)
        elif isinstance(expr, (PyCall, PyVar, PyInt)):
            yield interpreter_helper(expr, env)
        elif isinstance(expr, PyOp2):
            e1 = interpreter_helper(expr.e1, env)
            e2 = interpreter_helper(expr.e2, env)
            if expr.op == "+":
                yield e1 + e2
            elif expr.op == "-":
                yield e1 - e2
            elif expr.op == '*':
                yield e1 * e2
            elif expr.op == "/":
                yield e1 // e2
            else:
                raise Exception("Invalid Operation in Op2")
        else:
            raise Exception("Invalid Expr")




def interp_demo():
    env = empty_env()
    prog = [
        PyDefvar(PyVar('x'), PyInt(10)),
        PyDefvar(PyVar('y'), PyInt(42)),
        PyDefvar(PyVar('z'), PyInt(11)),
        PyVar('x'),
        PyVar('z'),
        PyVar('y'),
        PyOp2('+', PyInt(10), PyInt(32)),
        PyDefun(PyVar('double'), [PyVar('x')], [PyOp2('+', PyVar('x'), PyVar('x'))], env),
        PyCall(PyVar('double'), [PyInt(10)]),
        PyCall(PyVar('double'), [PyInt(21)]),
    ]

    for res in interpreter(prog, env):
        print(res)


if __name__ == '__main__':
    interp_demo()