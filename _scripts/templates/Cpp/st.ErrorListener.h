// Generated from trgen <version>

#pragma once

#include \<string>
#include \<iostream>
#include "ANTLRInputStream.h"
#include "CommonTokenStream.h"
#include "tree/ParseTree.h"
#include "tree/TerminalNode.h"
#include "tree/TerminalNodeImpl.h"
#include "misc/Interval.h"
#include "ConsoleErrorListener.h"

class ErrorListener : public antlr4::ConsoleErrorListener
{
    public:
        bool had_error;
        bool _quiet;
        bool _tee;
        std::ostream* _output;
    public:
        ErrorListener(bool quiet, bool tee, std::ostream* output);
        void syntaxError(antlr4::Recognizer* recognizer, antlr4::Token* offendingSymbol, size_t line, size_t col, const std::string& msg, std::exception_ptr e) override;
};
