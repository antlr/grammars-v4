#pragma once

#include "antlr4-runtime.h"

class JavaParserBase : public antlr4::Parser {
public:
    JavaParserBase(antlr4::TokenStream *input) : Parser(input) { }
    bool DoLastRecordComponent();
    bool IsNotIdentifierAssign();
};
