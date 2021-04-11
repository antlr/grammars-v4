// Template generated code from Antlr4BuildTasks.dotnet-antlr v <version>

#include "ErrorListener.h"

void ErrorListener::syntaxError(antlr4::Recognizer* recognizer, antlr4::Token* offendingSymbol, size_t line, size_t col, const std::string& msg, std::exception_ptr e)
{
    had_error = true;
    antlr4::ConsoleErrorListener::syntaxError(recognizer, offendingSymbol, line, col, msg, e);
}
