// Generated from trgen <version>

#include "ErrorListener.h"

ErrorListener::ErrorListener(bool quiet, bool tee, std::ostream* output)
{
    had_error = false;
    _quiet = quiet;
    _tee = tee;
    _output = output;
}

void ErrorListener::syntaxError(antlr4::Recognizer* recognizer, antlr4::Token* offendingSymbol, size_t line, size_t col, const std::string& msg, std::exception_ptr e)
{
    had_error = true;
    if (_tee)
    {
        (*_output) \<\< "line " \<\< line \<\< ":" \<\< col \<\< " " \<\< msg \<\< std::endl;
    }
    if (!_quiet)
    {
        std::cerr \<\< "line " \<\< line \<\< ":" \<\< col \<\< " " \<\< msg \<\< std::endl;
    }
}
