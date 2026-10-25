#pragma once
#include "antlr4-runtime.h"

class PlSqlLexerBase : public antlr4::Lexer
{
public:
  PlSqlLexerBase(antlr4::CharStream *input) : Lexer(input) { };

public:
  bool IsNewlineAtPos(int pos)
  {
    int la = _input->LA(pos);
    return la == -1 || la == '\n';
  };

  bool IsQQuoteDelimiter()
  {
    ssize_t offset = static_cast<ssize_t>(tokenStartCharIndex) + 2 - static_cast<ssize_t>(getCharIndex());
    int opening = _input->LA(offset);
    int closing = _input->LA(-2);
    if (opening == '[') return closing == ']';
    if (opening == '{') return closing == '}';
    if (opening == '<') return closing == '>';
    if (opening == '(') return closing == ')';
    return closing == opening;
  };
};
