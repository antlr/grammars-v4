#include "antlr4-runtime.h"
#include "LuaLexerBase.h"
#include "LuaLexer.h"

LuaLexerBase::LuaLexerBase(antlr4::CharStream * input) : antlr4::Lexer(input)
{
    _input = input;
}

void LuaLexerBase::HandleComment()
{
    auto cs = (antlr4::CharStream*)this->_input;
    this->start_line = this->getLine();
    this->start_col = this->getCharPositionInLine() - 2;
    if (cs->LA(1) == '[')
    {
        int sep = skip_sep(cs);
        if (sep >= 2)
        {
            read_long_string(cs, sep);
            return;
        }
    }
    while (cs->LA(1) != '\n' && cs->LA(1) != -1)
    {
        cs->consume();
    }
}

void LuaLexerBase::read_long_string(antlr4::CharStream * cs, int sep)
{
    bool done = false;
    cs->consume();
    for (; ; )
    {
        auto c = cs->LA(1);
        auto cc = (char)c;
        switch (c)
        {
            case (size_t)-1: {
                done = true;
                antlr4::ANTLRErrorListener & listener = this->getErrorListenerDispatch();
                listener.syntaxError(this, nullptr, this->start_line, this->start_col, "unfinished long comment", nullptr);
                break;
                }
            case ']':
                if (skip_sep(cs) == sep)
                {
                    cs->consume();
                    done = true;
                }
                break;
            default:
                if (cs->LA(1) == (size_t) -1)
                {
                    done = true;
                    break;
                }
                cs->consume();
                break;
        }
        if (done) break;
    }
}

int LuaLexerBase::skip_sep(antlr4::CharStream * cs)
{
    int count = 0;
    size_t s = cs->LA(1);
    char ss = (char)s;
    cs->consume();
    while (cs->LA(1) == '=')
    {
        cs->consume();
        count++;
    }
    if (cs->LA(1) == s) count += 2;
    else if (count == 0) count = 1;
    else count = 0;
    return count;
}

bool LuaLexerBase::IsLine1Col0()
{
    auto cs = (antlr4::CharStream*)this->_input;
    if (cs->index() == 1) return true;
    return false;
}
