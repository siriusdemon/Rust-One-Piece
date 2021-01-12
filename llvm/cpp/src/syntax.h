#include <string>
#include <vector>
#include <memory>

using std::string;
using std::vector;
using std::unique_ptr;


class ExprAst {
public:
    virtual ~ExprAst() {}
};

class NumberExprAst : public ExprAst {
    double val;

public:
    NumberExprAst(double val) : val(val) {}
};

class VariableExprAst : public ExprAst {
    string name;

public:
    VariableExprAst(const string &name) : name(name) {}
};

class BinaryExprAst : public ExprAst {
    char op;
    std::unique_ptr<ExprAst> lhs, rhs;

public:
    BinaryExprAst(char op, unique_ptr<ExprAst> lhs, unique_ptr<ExprAst> rhs) 
        : op(op), lhs(std::move(lhs)), rhs(std::move(rhs)) {}
    
};

// function call expr
class CallExprAst : public ExprAst {
    string callee;
    vector<unique_ptr<ExprAst>> args;

public:
    CallExprAst(const string &callee, vector<unique_ptr<ExprAst>> args)
        : callee(callee), args(std::move(args)) {}
};


// function declare (prototype)
class PrototypeAst {
    string name;
    vector<string> args;

public:
    PrototypeAst(const string &name, vector<string> args) 
        : name(name), args(std::move(args)) {}
    
    const string& getName() const { return name; }
};

class FunctionAst {
    unique_ptr<PrototypeAst> proto;
    unique_ptr<ExprAst> body;

public:
    FunctionAst(unique_ptr<PrototypeAst> proto, unique_ptr<ExprAst> body)
        : proto(std::move(proto)), body(std::move(body)) {}
};

