#include "antlr4-runtime.h"
#include "CSharpLexerBase.h"
#include "CSharpLexer.h"
#include <cctype>

CSharpLexerBase::CSharpLexerBase(antlr4::CharStream *input)
    : antlr4::Lexer(input)
{
}

// -------------------------------------------------------------------------
// Interpolated string helpers
// -------------------------------------------------------------------------
void CSharpLexerBase::OnInterpolatedRegularStringStart()
{
    interpolatedStringLevel++;
    interpolatedVerbatiums.push_back(false);
    verbatium = false;
}

void CSharpLexerBase::OnInterpolatedVerbatiumStringStart()
{
    interpolatedStringLevel++;
    interpolatedVerbatiums.push_back(true);
    verbatium = true;
}

void CSharpLexerBase::OnOpenBrace()
{
    if (interpolatedStringLevel > 0)
        curlyLevels.back()++;
}

void CSharpLexerBase::OnCloseBrace()
{
    if (interpolatedStringLevel > 0)
    {
        curlyLevels.back()--;
        if (curlyLevels.back() == 0)
        {
            curlyLevels.pop_back();
            skip();
            popMode();
        }
    }
}

void CSharpLexerBase::OnColon()
{
    if (interpolatedStringLevel > 0)
    {
        int ind = 1;
        bool switchToFormatString = true;
        auto *stream = dynamic_cast<antlr4::CharStream *>(getInputStream());
        while (stream->LA(ind) != '}')
        {
            auto ch = stream->LA(ind);
            if (ch == ':' || ch == ')')
            {
                switchToFormatString = false;
                break;
            }
            ind++;
        }
        if (switchToFormatString)
            setMode(CSharpLexer::INTERPOLATION_FORMAT);
    }
}

void CSharpLexerBase::OpenBraceInside()
{
    curlyLevels.push_back(1);
}

void CSharpLexerBase::OnDoubleQuoteInside()
{
    interpolatedStringLevel--;
    if (!interpolatedVerbatiums.empty())
        interpolatedVerbatiums.pop_back();
    verbatium = !interpolatedVerbatiums.empty() && interpolatedVerbatiums.back();
}

void CSharpLexerBase::OnCloseBraceInside()
{
    if (!curlyLevels.empty())
        curlyLevels.pop_back();
}

bool CSharpLexerBase::IsRegularCharInside() { return !verbatium; }
bool CSharpLexerBase::IsVerbatiumDoubleQuoteInside() { return verbatium; }

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
        case CSharpLexer::DEFINE: handleDefine(); break;
        case CSharpLexer::UNDEF:  handleUndef();  break;
        case CSharpLexer::IF:     skipped = handleIf();   break;
        case CSharpLexer::ELIF:   skipped = handleElif(); break;
        case CSharpLexer::ELSE:   skipped = handleElse(); break;
        case CSharpLexer::ENDIF:  handleEndif();  break;
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
        if (raw->getChannel() != antlr4::Token::HIDDEN_CHANNEL
            && raw->getChannel() != CSharpLexer::COMMENTS_CHANNEL)
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
    auto *stream = dynamic_cast<antlr4::CharStream *>(getInputStream());
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
    auto *stream = dynamic_cast<antlr4::CharStream *>(getInputStream());
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
    while (peekType() == CSharpLexer::OP_OR) { eConsume(); v = parseAnd() || v; }
    return v;
}

bool CSharpLexerBase::parseAnd()
{
    bool v = parseEq();
    while (peekType() == CSharpLexer::OP_AND) { eConsume(); v = parseEq() && v; }
    return v;
}

bool CSharpLexerBase::parseEq()
{
    bool v = parseUnary();
    if (peekType() == CSharpLexer::OP_EQ) { eConsume(); return v == parseUnary(); }
    if (peekType() == CSharpLexer::OP_NE) { eConsume(); return v != parseUnary(); }
    return v;
}

bool CSharpLexerBase::parseUnary()
{
    if (peekType() == CSharpLexer::BANG) { eConsume(); return !parseUnary(); }
    return parsePrimary();
}

bool CSharpLexerBase::parsePrimary()
{
    int t = peekType();
    if (t == CSharpLexer::TRUE)               { eConsume(); return true; }
    if (t == CSharpLexer::FALSE)              { eConsume(); return false; }
    if (t == CSharpLexer::CONDITIONAL_SYMBOL) { return symbols_.count(eConsume()->getText()) > 0; }
    if (t == CSharpLexer::OPEN_PARENS)
    {
        eConsume();
        bool v = parseOr();
        if (peekType() == CSharpLexer::CLOSE_PARENS) eConsume();
        return v;
    }
    return false;
}
