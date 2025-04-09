#pragma once

#include "antlr4-runtime.h"

/**
 * All parser methods that used in grammar (p, prev, notLineTerminator, etc.)
 * should start with lower case char similar to parser rules.
 */
class GoParserBase : public antlr4::Parser {
private:
    const bool debug = false;
    std::set<std::string> table;

public:
    GoParserBase(antlr4::TokenStream* input) : Parser(input) { }
    virtual ~GoParserBase() {}

protected:
    void myreset();
    bool closingBracket();
    bool isNotReceive();
    void addImportSpec();
    bool isOperand();
    bool isConversion();
    bool isMethodExpr();
};
