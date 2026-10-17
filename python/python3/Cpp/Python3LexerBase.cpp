#include "Python3Lexer.h"
#include "Python3Parser.h"
#include <regex>

using namespace antlr4;

Python3LexerBase::Python3LexerBase(antlr4::CharStream *input): Lexer(input)
{
    opened = 0;
    lastToken = nullptr;
}

void Python3LexerBase::emit(std::unique_ptr<antlr4::Token> t)
{
    tokens.push_back(cloneToken(t));
    setToken(std::move(t));
}


std::unique_ptr<antlr4::Token> Python3LexerBase::nextToken()
{
    if (_input->LA(1) == EOF && !indents.empty()) {
        for (int i = tokens.size() - 1; i >= 0; i--) {
            if (tokens[i]->getType() == EOF) {
                tokens.erase(tokens.begin() + i);
            }
        }
        emit(commonToken(Python3Parser::NEWLINE, "\n"));
        while (!indents.empty()) {
            emit(createDedent());
            indents.pop();
        }
        emit(commonToken(EOF, "<EOF>"));
    }
    std::unique_ptr<antlr4::Token> next = Lexer::nextToken();
    if (next->getChannel() == antlr4::Token::DEFAULT_CHANNEL) {
        lastToken = cloneToken(next);
    }
    if (!tokens.empty())
    {
        next = std::move(*tokens.begin());
        tokens.erase(tokens.begin());
    }
    return next;
}

std::unique_ptr<antlr4::Token> Python3LexerBase::createDedent() {
    std::unique_ptr<antlr4::CommonToken> dedent = commonToken(Python3Parser::DEDENT, "");
    return dedent;
}

std::unique_ptr<antlr4::CommonToken> Python3LexerBase::commonToken(size_t type, const std::string& text) {
    int stop = getCharIndex() - 1;
    int start = text.empty() ? stop : stop - text.size() + 1;
    return _factory->create({ this, _input }, type, text, DEFAULT_TOKEN_CHANNEL, start, stop, lastToken ? lastToken->getLine() : 0, lastToken ? lastToken->getCharPositionInLine() : 0);
}

std::unique_ptr<antlr4::CommonToken> Python3LexerBase::cloneToken(const std::unique_ptr<antlr4::Token>& source) {
    return _factory->create({ this, _input }, source->getType(), source->getText(), source->getChannel(), source->getStartIndex(), source->getStopIndex(), source->getLine(), source->getCharPositionInLine());
}

int Python3LexerBase::getIndentationCount(const std::string& spaces) {
    int count = 0;
    for (char ch : spaces) {
        switch (ch) {
            case '\t':
                count += 8 - (count % 8);
                break;
            default:
      // A normal space char.
                count++;
        }
    }

    return count;
}

bool Python3LexerBase::atStartOfInput() {
    return getCharPositionInLine() == 0 && getLine() == 1;
}

void Python3LexerBase::openBrace() {
    this->opened++;
}

void Python3LexerBase::closeBrace() {
    this->opened--;
}


void Python3LexerBase::onNewLine()
{
    std::string newLine = std::regex_replace(getText(), std::regex("[^\r\n\f]+"), "");
    std::string spaces = std::regex_replace(getText(), std::regex("[\r\n\f]+"), "");
    int next = _input->LA(1);
    int nextnext = _input->LA(2);
    if (opened > 0 || (nextnext != -1 && (next == '\r' || next == '\n' || next == '\f' || next == '#'))) {
        skip();
    }
    else {
        emit(commonToken(Python3Lexer::NEWLINE, newLine));
        int indent = getIndentationCount(spaces);
        int previous = indents.empty() ? 0 : indents.top();
        if (indent == previous) {
            skip();
        }
        else if (indent > previous) {
            indents.push(indent);
            emit(commonToken(Python3Lexer::INDENT, spaces));
        }
        else {
            while(!indents.empty() && indents.top() > indent) {
                emit(createDedent());
                indents.pop();
            }
        }
    }
}


void Python3LexerBase::reset()
{
    tokens = std::vector<std::unique_ptr<antlr4::Token>>{};
    indents = std::stack<int>{};
    opened = 0;
    lastToken = nullptr;
    Lexer::reset();
}
