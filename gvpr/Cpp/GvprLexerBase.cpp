#include "antlr4-runtime.h"
#include "GvprLexerBase.h"
#include "gvprLexer.h"

GvprLexerBase::GvprLexerBase(antlr4::CharStream * input) : antlr4::Lexer(input)
{
    _input = input;
}

bool GvprLexerBase::IsColumnZero()
{
    return this->getCharPositionInLine() == 1;
}
