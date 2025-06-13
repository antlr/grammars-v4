/* eslint-disable no-underscore-dangle */
/* cspell: ignore antlr, longlong, ULONGLONG, MAXDB */

#pragma once

#include "antlr4-runtime.h"
#include "LuaParserBase.h"
#include "LuaParser.h"

LuaParserBase::LuaParserBase(antlr4::TokenStream* input) : Parser(input)
{
}

bool LuaParserBase::IsFunctionCall()
{
    antlr4::BufferedTokenStream* stream = static_cast<antlr4::BufferedTokenStream*>(_input);
    auto la = stream->LT(1);
    if (la->getType() != LuaParser::NAME) return false;
    la = stream->LT(2);
    if (la->getType() == LuaParser::OP) return false;
    return true;
}
