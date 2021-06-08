// Template generated code from Antlr4BuildTasks.dotnet-antlr v <version>

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
#include "CharStream.h"

namespace antlr4 {
namespace runtime {

    /// \<summary>
    /// This class supports case-insensitive lexing by wrapping an existing
    /// \<see cref=""ICharStream""/> and forcing the lexer to see either upper or
    /// lowercase characters. Grammar literals should then be either upper or
    /// lower case such as 'BEGIN' or 'begin'. The text of the character
    /// stream is unaffected. Example: input 'BeGiN' would match lexer rule
    /// 'BEGIN' if constructor parameter upper=true but getText() would return
    /// 'BeGiN'.
    /// \</summary>
    class CaseChangingCharStream : public antlr4::CharStream
    {
    private:
        antlr4::CharStream* stream;
    private:
        bool upper;

        /// \<summary>
        /// Constructs a new CaseChangingCharStream wrapping the given \<paramref name=""stream""/> forcing
        /// all characters to upper case or lower case.
        /// \</summary>
        /// \<param name=""stream"">The stream to wrap.\</param>
        /// \<param name=""upper"">If true force each symbol to upper
        /// case, otherwise force to lower.\</param>
    public:
        CaseChangingCharStream(antlr4::CharStream* stream, bool upper);
	virtual size_t index();
	virtual size_t size();
	virtual std::string getSourceName() const;
	virtual void consume();
	virtual std::string getText(const antlr4::misc::Interval& interval);
	virtual size_t LA(ssize_t i);
	virtual ssize_t mark();
	virtual void release(ssize_t marker);
	virtual void seek(size_t index);
	virtual std::string toString() const;
    };
}
}