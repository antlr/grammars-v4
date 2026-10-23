#include "antlr4-runtime.h"
#include "CSharpLexerBase.h"
#include "CSharpLexer.h"
#include <algorithm>
#include <cctype>
#include <iostream>

CSharpLexerBase::CSharpLexerBase(antlr4::CharStream *input)
    : antlr4::Lexer(input), charStream_(input)
{
}

// -------------------------------------------------------------------------
// Mode-stack helpers
// -------------------------------------------------------------------------

int CSharpLexerBase::PeekMode() const
{
    return modeStack.empty() ? DEFAULT_MODE : static_cast<int>(modeStack.back());
}

size_t CSharpLexerBase::popMode()
{
    if (modeStack.empty())
    {
        std::cerr << "unbalanced ()/{}/[]\n";
        return DEFAULT_MODE;
    }
    return antlr4::Lexer::popMode();
}

bool CSharpLexerBase::PeekModeIs(int mode) const
{
    return PeekMode() == mode;
}

bool CSharpLexerBase::LookAheadIs(int pos, int value)
{
    return charStream_->LA(pos) == value;
}

bool CSharpLexerBase::LookAheadIsNot(int pos, int value)
{
    return charStream_->LA(pos) != value;
}

bool CSharpLexerBase::LookAheadIsRBrace1() const
{
    return charStream_->LA(1) == '}';
}

bool CSharpLexerBase::LookAheadIsNotLBrace2() const
{
    return charStream_->LA(2) != '{';
}

bool CSharpLexerBase::PeekModeIsIrsCont() const
{
    return PeekModeIs(CSharpLexer::IRS_CONT);
}

bool CSharpLexerBase::PeekModeIsIvsCont() const
{
    return PeekModeIs(CSharpLexer::IVS_CONT);
}

void CSharpLexerBase::WrapToken()
{
    std::string text = getText();
    // U+3014 = \xe3\x80\x94, U+3015 = \xe3\x80\x95 (UTF-8)
    static const std::string U3015_UTF8 = "\xe3\x80\x95";
    static const std::string OPEN_UTF8  = "\xe3\x80\x94";
    static const std::string CLOSE_UTF8 = "\xe3\x80\x95";

    std::string replaced;
    for (size_t i = 0; i < text.size(); )
    {
        if (i + 3 <= text.size()
            && (unsigned char)text[i]   == 0xe3
            && (unsigned char)text[i+1] == 0x80
            && (unsigned char)text[i+2] == 0x95)
        {
            replaced += U3015_UTF8;
            replaced += U3015_UTF8;
            i += 3;
        }
        else
        {
            replaced += text[i++];
        }
    }
    setText(OPEN_UTF8 + replaced + CLOSE_UTF8);
}

bool CSharpLexerBase::isActive() const
{
    return condition_.empty() || condition_.back();
}

// -------------------------------------------------------------------------
// nextToken override — intercepts DIRECTIVE-channel tokens
// -------------------------------------------------------------------------
std::unique_ptr<antlr4::Token> CSharpLexerBase::nextToken()
{
    if (!pending_.empty())
    {
        auto tok = std::move(pending_.front());
        pending_.pop_front();
        return tok;
    }

    auto tok = antlr4::Lexer::nextToken();

    if (tok->getChannel() == CSharpLexer::DIRECTIVE)
    {
        std::unique_ptr<antlr4::Token> skipped;
        switch (tok->getType())
        {
        case CSharpLexer::DEFINE:   handleDefine(); break;
        case CSharpLexer::UNDEF:    handleUndef();  break;
        case CSharpLexer::KW_IF:    skipped = handleIf();   break;
        case CSharpLexer::ELIF:     skipped = handleElif(); break;
        case CSharpLexer::KW_ELSE:  skipped = handleElse(); break;
        case CSharpLexer::ENDIF:    handleEndif();  break;
        }
        if (skipped)
            pending_.push_back(std::move(skipped));
    }

    return tok;
}

// -------------------------------------------------------------------------
// Directive handlers
// -------------------------------------------------------------------------
void CSharpLexerBase::handleDefine()
{
    auto line = collectLine();
    auto sym = symbolFromLine(line);
    if (isActive() && !sym.empty()) symbols_.insert(sym);
}

void CSharpLexerBase::handleUndef()
{
    auto line = collectLine();
    auto sym = symbolFromLine(line);
    if (isActive() && !sym.empty()) symbols_.erase(sym);
}

std::unique_ptr<antlr4::Token> CSharpLexerBase::handleIf()
{
    auto line = collectLine();
    bool outer = isActive();
    bool result = outer && evaluate(line);
    condition_.push_back(result);
    taken_.push_back(result);
    return result ? nullptr : skipFalseBlock();
}

std::unique_ptr<antlr4::Token> CSharpLexerBase::handleElif()
{
    auto line = collectLine();
    bool alreadyTaken = !taken_.empty() ? taken_.back() : false;
    if (!taken_.empty()) taken_.pop_back();
    if (!condition_.empty()) condition_.pop_back();
    bool outer = isActive();
    bool result = !alreadyTaken && outer && evaluate(line);
    condition_.push_back(result);
    taken_.push_back(alreadyTaken || result);
    return result ? nullptr : skipFalseBlock();
}

std::unique_ptr<antlr4::Token> CSharpLexerBase::handleElse()
{
    collectLine();
    bool alreadyTaken = !taken_.empty() ? taken_.back() : false;
    if (!taken_.empty()) taken_.pop_back();
    if (!condition_.empty()) condition_.pop_back();
    bool outer = isActive();
    bool result = !alreadyTaken && outer;
    condition_.push_back(result);
    taken_.push_back(true);
    return result ? nullptr : skipFalseBlock();
}

void CSharpLexerBase::handleEndif()
{
    collectLine();
    if (!condition_.empty()) condition_.pop_back();
    if (!taken_.empty())    taken_.pop_back();
}

// -------------------------------------------------------------------------
// collectLine — drain DIRECTIVE_MODE tokens; tokens owned by lineStorage_
// -------------------------------------------------------------------------
std::vector<antlr4::Token *> CSharpLexerBase::collectLine()
{
    lineStorage_.clear();
    std::vector<antlr4::Token *> result;
    while (true)
    {
        auto t = antlr4::Lexer::nextToken();
        antlr4::Token *raw = t.get();
        if (raw->getChannel() != antlr4::Token::HIDDEN_CHANNEL)
            result.push_back(raw);
        bool done = (raw->getType() == CSharpLexer::DIRECTIVE_NEW_LINE
                  || raw->getType() == antlr4::Token::EOF);
        lineStorage_.push_back(std::move(t));
        if (done) break;
    }
    return result;
}

std::string CSharpLexerBase::symbolFromLine(const std::vector<antlr4::Token *> &line)
{
    for (auto *t : line)
        if (t && t->getType() == CSharpLexer::CONDITIONAL_SYMBOL)
            return t->getText();
    return "";
}

// -------------------------------------------------------------------------
// skipFalseBlock — scan char stream, return SKIPPED_SECTION on HIDDEN channel
// -------------------------------------------------------------------------
std::unique_ptr<antlr4::Token> CSharpLexerBase::skipFalseBlock()
{
    std::string text;
    auto *stream = charStream_;
    int depth = 1;
    bool atLineStart = true;
    size_t startLine = getLine();

    while (true)
    {
        int c = stream->LA(1);
        if (c == antlr4::CharStream::EOF) break;

        if (c == '\r' || c == '\n' || c == 0x85 || c == 0x2028 || c == 0x2029)
        {
            stream->consume();
            text += static_cast<char>(c);
            if (c == '\r' && stream->LA(1) == '\n')
            {
                stream->consume();
                text += '\n';
            }
            atLineStart = true;
            continue;
        }

        if (atLineStart && (c == ' ' || c == '\t'))
        {
            stream->consume();
            text += static_cast<char>(c);
            continue;
        }

        if (atLineStart && c == '#')
        {
            std::string kw = peekKeyword();
            if (kw == "if")
                depth++;
            else if (kw == "endif")
            {
                if (--depth == 0) break;
            }
            else if ((kw == "else" || kw == "elif") && depth == 1)
                break;
        }

        atLineStart = false;
        stream->consume();
        text += static_cast<char>(c);
    }

    auto tok = _factory->create(
        {this, getInputStream()},
        CSharpLexer::SKIPPED_SECTION,
        text,
        antlr4::Token::HIDDEN_CHANNEL,
        -1, -1,
        static_cast<int>(startLine), 0);
    return tok;
}

std::string CSharpLexerBase::peekKeyword()
{
    auto *stream = charStream_;
    int i = 2; // LA(1) is '#'
    while (stream->LA(i) == ' ' || stream->LA(i) == '\t') i++;
    std::string kw;
    int c;
    while ((c = stream->LA(i)) != antlr4::CharStream::EOF && std::isalpha(c))
    {
        kw += static_cast<char>(c);
        i++;
    }
    return kw;
}

// -------------------------------------------------------------------------
// Recursive-descent expression evaluator
// -------------------------------------------------------------------------
bool CSharpLexerBase::evaluate(const std::vector<antlr4::Token *> &tokens)
{
    exprTokens_ = tokens;
    epos_ = 0;
    return parseOr();
}

int CSharpLexerBase::peekType()
{
    if (epos_ < static_cast<int>(exprTokens_.size()))
    {
        int t = exprTokens_[epos_]->getType();
        if (t != CSharpLexer::DIRECTIVE_NEW_LINE && t != antlr4::Token::EOF)
            return t;
    }
    return -1;
}

antlr4::Token *CSharpLexerBase::eConsume() { return exprTokens_[epos_++]; }

bool CSharpLexerBase::parseOr()
{
    bool v = parseAnd();
    while (peekType() == CSharpLexer::TK_OR_OR) { eConsume(); v = parseAnd() || v; }
    return v;
}

bool CSharpLexerBase::parseAnd()
{
    bool v = parseEq();
    while (peekType() == CSharpLexer::TK_AND_AND) { eConsume(); v = parseEq() && v; }
    return v;
}

bool CSharpLexerBase::parseEq()
{
    bool v = parseUnary();
    if (peekType() == CSharpLexer::TK_EQ_EQ) { eConsume(); return v == parseUnary(); }
    if (peekType() == CSharpLexer::TK_NOT_EQ) { eConsume(); return v != parseUnary(); }
    return v;
}

bool CSharpLexerBase::parseUnary()
{
    if (peekType() == CSharpLexer::TK_NOT) { eConsume(); return !parseUnary(); }
    return parsePrimary();
}

bool CSharpLexerBase::parsePrimary()
{
    int t = peekType();
    if (t == CSharpLexer::TRUE)               { eConsume(); return true; }
    if (t == CSharpLexer::FALSE)              { eConsume(); return false; }
    if (t == CSharpLexer::CONDITIONAL_SYMBOL) { return symbols_.count(eConsume()->getText()) > 0; }
    if (t == CSharpLexer::TK_LPAREN)
    {
        eConsume();
        bool v = parseOr();
        if (peekType() == CSharpLexer::TK_RPAREN) eConsume();
        return v;
    }
    return false;
}
