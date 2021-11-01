#ifndef PLSQLLEXERBASE_H
#define PLSQLLEXERBASE_H

#include "antlr4-runtime.h"

class PlSqlLexerBase : public antlr4::Lexer
{
public:
  PlSqlLexerBase(antlr4::CharStream *input) : Lexer(input), self(*this) { }

public:
  PlSqlLexerBase & self;

public:
  bool IsNewlineAtPos(int pos)
  {
    int la = _input->LA(pos);
    return la == -1 || la == '\n';
  }
};

#endif