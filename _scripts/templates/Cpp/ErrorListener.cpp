// Template generated code from trgen <version>

#include "ErrorListener.h"

void ErrorListener::syntaxError(antlr4::Recognizer* recognizer, antlr4::Token* offendingSymbol, size_t line, size_t col, const std::string& msg, std::exception_ptr e)
{
    had_error = true;
    std::cout \<\< "line " + line + ":" + charPositionInLine + " " + msg \<\< std::endl;
}
