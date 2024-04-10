#include "antlr4-runtime.h"
#include "Dart2LexerBase.h"
#include "Dart2Lexer.h"

Dart2LexerBase::Dart2LexerBase(antlr4::CharStream * input) : antlr4::Lexer(input)
{
    _input = input;
}

bool Dart2LexerBase::CheckNotOpenBrace()
{
    return this->_input->LA(1) != '{';
}
