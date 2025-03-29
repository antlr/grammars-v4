#pragma once

#include "antlr4-runtime.h"

/**
 * All parser methods that used in grammar (p, prev, notLineTerminator, etc.)
 * should start with lower case char similar to parser rules.
 */
class GoParserBase : public antlr4::Parser {
public:
    GoParserBase(antlr4::TokenStream* input) : Parser(input) {
    }

    virtual ~GoParserBase() {}

protected:
    /**
     * Returns true if the current Token is a closing bracket (")" or "}")
     */
    bool closingBracket();

    bool isType();
    bool isNotReceive();
};
