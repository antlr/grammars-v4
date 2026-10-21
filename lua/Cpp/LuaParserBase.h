/* eslint-disable no-underscore-dangle */
/* cspell: ignore antlr, longlong, ULONGLONG, MAXDB */

#pragma once

#include "antlr4-runtime.h"

class LuaParserBase : public antlr4::Parser {

    protected:
        LuaParserBase(antlr4::TokenStream* input);

    public:
        bool IsFunctionCall();
};
