#pragma once

#include "antlr4-runtime.h"

/**
 * All parser methods that used in grammar (p, prev, notLineTerminator, etc.)
 * should start with lower case char similar to parser rules.
 */
class GoParserBase : public antlr4::Parser {
private:
    bool debug = false;
    std::set<std::string> table;

    static bool hasArg(int argc, char* argv[], const std::string& arg);

public:
    GoParserBase(antlr4::TokenStream* input) : Parser(input) { }
    GoParserBase(antlr4::TokenStream* input, int argc, char* argv[]);
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
