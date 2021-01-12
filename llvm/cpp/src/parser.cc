#include "parser.h"


// ------------------------ scanning
int gettok() {
    static int LastChar = ' ';

    // skip whitespace
    while (isspace(LastChar)) {
        LastChar = getchar();
    }

    // symbol
    if (isalpha(LastChar)) {
        IdentifierStr = LastChar;
        LastChar = getchar();
        while (isalnum(LastChar)) {
            IdentifierStr += LastChar;
            LastChar = getchar();
        }

        if (IdentifierStr == "def") {
            return tok_def;
        }
        if (IdentifierStr == "extern") {
            return tok_extern;
        }
        return tok_identifier;
    }

    // number
    if (isdigit(LastChar) || LastChar == '.') {
        std::string NumStr;
        do {
            NumStr += LastChar;
            LastChar = getchar();
        } while (isdigit(LastChar) || LastChar == '.');

        NumVal = strtod(NumStr.c_str(), 0);
        return tok_number;
    }

    // comment
    if (LastChar == '#') {
        do {
            LastChar = getchar();
        } while (LastChar != EOF && LastChar != '\n' && LastChar != '\r');
        
        if (LastChar != EOF) {
            return gettok();
        }
    }

    if (LastChar == EOF) {
        return tok_eof;
    }

    // not all the cases above, is operator like '+', '*'
    int OpChar = LastChar;
    LastChar = getchar();
    return OpChar;
}

static int CurTok;
static int getNextToken() {
    CurTok = gettok();
    return CurTok;
}


// ------------------------ parsing
// I should say, the piece of code is hard to understand
// always take that:
// NumVal is a global variable that holds the current number
unique_ptr<ExprAst> parseNumber() {
    auto result = std::make_unique<NumberExprAst>(NumVal);
    getNextToken();
    return std::move(result);
}

// when encounter an (, we firstly eat it and parse the exp
// in (), finally we check the ) exists
unique_ptr<ExprAst> parseParen() {
    getNextToken(); // eat (
    auto v = parseExpression(); 
    if (!V) { return nullptr; }
    if (CurTok != ')') {
        return logError("expected ')'");
    }
    getNextToken(); // eat }
    return v;
}

unique_ptr<ExprAst> parseIdentifier() {
    string symbol = IdentifierStr;
    getNextToken();

    // we need to distingish variable and function call
    // firstly handle variable
    if (CurTok != '(') {
        return std::make_unique<VariableExprAst>(symbol);
    }
    // then funcall
    getNextToken(); // eat '('
    vector<unique_ptr<ExprAst>> args;
    if (CurTok != ')') {
        while (true) {
            auto arg = parseExpression();
            if (!arg) { return nullptr; }
            args.push_back(std::move(arg));

            if (CurTok == ')') break;
            if (CurTok != ',') {
                return logError("Expected ')' or ',' in argument list");
            }
            getNextToken();
        }
    }
    getNextToken();
    return std::make_unique<CallExprAst>(symbol, std::move(args));
}


unique_ptr<ExprAst> parsePrimary() {
    switch (CurTok) {
    default:
        return logError("unknown token when expecting an expression.");
    case tok_identifier:
        return parseIdentifier();
    case tok_number:
        return parseNumber();
    case '(':
        return parseParen();
    }
}

// now parse binary expression
static std::map<char, int> PrecedenceTable;

void init_precedence_table() {
    PrecedenceTable['<'] = 10;
    PrecedenceTable['+'] = 20;
    PrecedenceTable['-'] = 20;
    PrecedenceTable['*'] = 40;
    PrecedenceTable['/'] = 40;
}

int getTokPrecedence() {
    if (!isascii(CurTok)) {
        return -1;
    }
    int tok_prec = PrecedenceTable[CurTok];
    if (tok_prec <= 0) { return -1; }
    return tok_prec;
}

// read the docs
//  it will first parse the leading primary expression “a”, then it will see the pairs 
//  [+, b] [+, (c+d)] [*, e] [*, f] and [+, g].
// this is the hardest part
// I can make it better afterward.
unique_ptr<ExprAst> parseExpression() {
    auto lhs = parsePrimary();
    if (!lhs) { return nullptr; }

    return parseBinOpRHS(0, std::move(lhs));
}

unique_ptr<ExprAst> parseBinOpRHS(int prec, unique_ptr<ExprAst> lhs) {
    while (true)
    {
        int tok_prec = getTokPrecedence();
        if (tok_prec < prec) {
            return lhs;
        }
        int op = CurTok;
        getNextToken();
        auto rhs = parsePrimary();
        if (!rhs) { return nullptr; }
        int next_prec = getTokPrecedence();
        // 这里的意思是结合律
        // lhs 要与谁结合？ 现在的这个 rhs 吗？ 还是 rhs 先跟后面的式子结合，再回头跟
        // lhs 结合，这取决于运算符的优先级
        if (tok_prec < next_prec) {
            rhs = parseBinOpRHS(tok_prec + 1, std::move(rhs));
            if (! rhs) { return nullptr; }
        }
        lhs = std::make_unique<BinaryExprAst>(op, std::move(lhs), std::move(rhs));
    }
}

