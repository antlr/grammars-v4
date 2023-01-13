#ifndef PLSQLPARSERBASE_H
#define PLSQLPARSERBASE_H

#include "antlr4-runtime.h"

class PlSqlParserBase : public antlr4::Parser
{
    bool _isVersion12 = true;
    bool _isVersion10 = true;
  public:
    PlSqlParserBase & self;

  public:
    PlSqlParserBase(antlr4::TokenStream *input) : Parser(input), self(*this) { }

    bool isVersion12()
    {
        return _isVersion12;
    }

    void setVersion12(bool value)
    {
        _isVersion12 = value;
    }

    bool isVersion10()
    {
        return _isVersion10;
    }

    void setVersion10(bool value)
    {
        _isVersion10 = value;
    }
};

#endif