#ifndef PLSQLBASELEXER_H
#define PLSQLBASELEXER_H

#include "antlr4-runtime.h"

class PlSqlBaseLexer : public antlr4::Lexer
{
public:
  PlSqlBaseLexer(antlr4::CharStream *input) : Lexer(input) {}

protected:
  bool IsNewlineAtPos(int pos)
  {
    int la = _input->LA(pos);
    return la == -1 || la == '\n';
  }
};

#endif