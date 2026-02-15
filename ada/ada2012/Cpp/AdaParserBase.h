#pragma once
#include "antlr4-runtime.h"

class AdaParserBase : public antlr4::Parser
{
public:
    AdaParserBase(antlr4::TokenStream * input);
    void ParsePragmas();
};
