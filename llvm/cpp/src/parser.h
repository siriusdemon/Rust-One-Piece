#include <string>
#include <memory>
#include <map>
#include "syntax.h"

using std::string;
using std::unique_ptr;
// why all negative
// answer is 
// Each token returned by our lexer will either be one of the 
// Token enum values or it will be an ‘unknown’ character like ‘+’, 
// which is returned as its ASCII value. 
enum Token {
    tok_eof = -1,

    // commands
    tok_def = -2,
    tok_extern = -3,

    // primary
    tok_identifier = -4,
    tok_number = -5,
};

static string IdentifierStr;
static double NumVal;


// function prototype in parser.cc
int gettok();
unique_ptr<ExprAst> parseNumber();
unique_ptr<ExprAst> parseParen();
unique_ptr<ExprAst> parseIdentifier();
unique_ptr<ExprAst> parsePrimary();
unique_ptr<ExprAst> parseExpression();
unique_ptr<ExprAst> parseBinOpRHS(int, unique_ptr<ExprAst>);

// helper
unique_ptr<ExprAst> logError(const char* str) {
    fprintf(stderr, "LogError: %s\n", str);
    return nullptr;
}

unique_ptr<PrototypeAst> logErrorP(const char* str) {
    logError(str);
    return nullptr;
}