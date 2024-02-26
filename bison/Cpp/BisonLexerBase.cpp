#include "antlr4-runtime.h"
#include "BisonLexerBase.h"
#include "BisonLexer.h"

BisonLexerBase::BisonLexerBase(antlr4::CharStream * input) : antlr4::Lexer(input)
{
    percent_percent_count = 0;
    _input = input;
}

void BisonLexerBase::NextMode()
{
    ++percent_percent_count;
    if (percent_percent_count == 1)
    {
        return;
    } else if (percent_percent_count == 2)
    {
        this->pushMode(BisonLexer::EpilogueMode);
        return;
    } else
    {
        this->setType(BisonLexer::PercentPercent);
        return;
    }
}

void BisonLexerBase::reset()
{
    percent_percent_count = 0;
    Lexer::reset();
}
