#include "antlr4-runtime.h"
#include "MySQLLexerBase.h"
#include "MySQLLexer.h"
#include "SqlMode.h"
#include "SqlModes.h"

#include <set>
#include <string>
#include <vector>
#include <sstream>
#include <algorithm>
#include <iostream>

MySQLLexerBase::MySQLLexerBase(antlr4::CharStream * input) : antlr4::Lexer(input)
{
    this->serverVersion = 80200;
    this->sqlModes = SqlModes::sqlModeFromString("ANSI_QUOTES");
    this->supportMle = true;
    this->inVersionComment = false;
}

bool MySQLLexerBase::isSqlModeActive(SqlMode mode) {
    return sqlModes.count(mode) > 0;
}

void MySQLLexerBase::reset() {
    inVersionComment = false;
    Lexer::reset();
}

std::unique_ptr<antlr4::Token> MySQLLexerBase::nextToken()
{
    if (!this->pendingTokens.empty()) {
        std::unique_ptr<antlr4::Token> pending = std::move(this->pendingTokens.front());
        this->pendingTokens.pop();
        return std::move(pending);
    }

    std::unique_ptr<antlr4::Token> next = std::move(Lexer::nextToken());
    if (!pendingTokens.empty()) {
        std::unique_ptr<antlr4::Token> pending = std::move(this->pendingTokens.front());
        this->pendingTokens.pop();
        this->pendingTokens.push(std::move(next));
        return std::move(pending);
    }

    return std::move(next);
}

bool MySQLLexerBase::checkMySQLVersion(std::string text) {
    if (text.length() < 8) return false;

    int version = std::stoi(text.substr(3));
    if (version <= serverVersion) {
        inVersionComment = true;
        return true;
    }

    return false;
}

int MySQLLexerBase::determineFunction(int proposed) {
    char input = static_cast<char>(_input->LA(1));
    if (isSqlModeActive(SqlMode::IgnoreSpace)) {
        while (isspace(input)) {
            this->getInterpreter<antlr4::atn::LexerATNSimulator>()->consume(_input);
            input = static_cast<char>(_input->LA(1));
        }
    }

    return (input == '(') ? proposed : MySQLLexer::IDENTIFIER;
}

int MySQLLexerBase::determineNumericType(std::string text) {
    int length = text.length() - 1;
    if (length < longLength) return MySQLLexer::INT_NUMBER;

    bool negative = false;
    int index = 0;

    if (text[index] == '+') {
        ++index; --length;
    } else if (text[index] == '-') {
        ++index; --length;
        negative = true;
    }

    while (text[index] == '0' && length > 0) {
        ++index; --length;
    }

    if (length < longLength) return MySQLLexer::INT_NUMBER;

    std::string cmp;
    int smaller, bigger;

    if (negative) {
        if (length == longLength) {
            cmp = signedLongString.substr(1);
            smaller = MySQLLexer::INT_NUMBER;
            bigger = MySQLLexer::LONG_NUMBER;
        } else if (length < signedLongLongLength) {
            return MySQLLexer::LONG_NUMBER;
        } else if (length > signedLongLongLength) {
            return MySQLLexer::DECIMAL_NUMBER;
        } else {
            cmp = signedLongLongString.substr(1);
            smaller = MySQLLexer::LONG_NUMBER;
            bigger = MySQLLexer::DECIMAL_NUMBER;
        }
    } else {
        if (length == longLength) {
            cmp = longString;
            smaller = MySQLLexer::INT_NUMBER;
            bigger = MySQLLexer::LONG_NUMBER;
        } else if (length < longLongLength) {
            return MySQLLexer::LONG_NUMBER;
        } else if (length > longLongLength) {
            if (length > unsignedLongLongLength) return MySQLLexer::DECIMAL_NUMBER;
            cmp = unsignedLongLongString;
            smaller = MySQLLexer::ULONGLONG_NUMBER;
            bigger = MySQLLexer::DECIMAL_NUMBER;
        } else {
            cmp = longLongString;
            smaller = MySQLLexer::LONG_NUMBER;
            bigger = MySQLLexer::ULONGLONG_NUMBER;
        }
    }

    int cmpIndex = 0;
    while (index < text.length() && text[index] == cmp[cmpIndex]) {
        ++index; ++cmpIndex;
    }

    return (text[index - 1] <= cmp[cmpIndex - 1]) ? smaller : bigger;
}

int MySQLLexerBase::checkCharset(std::string text) {
    return (charSets.count(text) > 0) ? MySQLLexer::UNDERSCORE_CHARSET : MySQLLexer::IDENTIFIER;
}

void MySQLLexerBase::emitDot() {
    auto len = this->getText().length();
    auto * t = new antlr4::CommonToken(
        std::pair<antlr4::TokenSource*, antlr4::CharStream*>(this, this->_input),
        MySQLLexer::DOT_SYMBOL, 0, this->tokenStartCharIndex, this->tokenStartCharIndex);
    t->setCharPositionInLine(t->getCharPositionInLine() - len);
    pendingTokens.push(std::make_unique<antlr4::CommonToken>(t));
    this->setCharPositionInLine(this->getCharPositionInLine() + 1);
    ++this->tokenStartCharIndex;
    this->justEmittedDot = true;
}

antlr4::Token* MySQLLexerBase::emit() {
    auto t = Lexer::emit();
    if (this->justEmittedDot) {
        antlr4::CommonToken* p = dynamic_cast<antlr4::CommonToken*>(t);
	p->setCharPositionInLine(p->getCharPositionInLine() + 1);
	this->setCharPositionInLine(this->getCharPositionInLine() - 1);
        this->justEmittedDot = false;
    }
    return t;
}

bool MySQLLexerBase::isMasterCompressionAlgorithm() { return this->serverVersion >= 80018 && this->isServerVersionLt80024(); }
bool MySQLLexerBase::isServerVersionGe80011() { return this->serverVersion >= 80011; }
bool MySQLLexerBase::isServerVersionGe80013() { return this->serverVersion >= 80013; }
bool MySQLLexerBase::isServerVersionLt80014() { return this->serverVersion < 80014; }
bool MySQLLexerBase::isServerVersionGe80014() { return this->serverVersion >= 80014; }
bool MySQLLexerBase::isServerVersionGe80016() { return this->serverVersion >= 80016; }
bool MySQLLexerBase::isServerVersionGe80017() { return this->serverVersion >= 80017; }
bool MySQLLexerBase::isServerVersionGe80018() { return this->serverVersion >= 80018; }
bool MySQLLexerBase::isServerVersionLt80021() { return this->serverVersion < 80021; }
bool MySQLLexerBase::isServerVersionGe80021() { return this->serverVersion >= 80021; }
bool MySQLLexerBase::isServerVersionLt80022() { return this->serverVersion < 80022; }
bool MySQLLexerBase::isServerVersionGe80022() { return this->serverVersion >= 80022; }
bool MySQLLexerBase::isServerVersionLt80023() { return this->serverVersion < 80023; }
bool MySQLLexerBase::isServerVersionGe80023() { return this->serverVersion >= 80023; }
bool MySQLLexerBase::isServerVersionLt80024() { return this->serverVersion < 80024; }
bool MySQLLexerBase::isServerVersionGe80024() { return this->serverVersion >= 80024; }
bool MySQLLexerBase::isServerVersionLt80031() { return this->serverVersion < 80031; }
void MySQLLexerBase::doLogicalOr() { this->type = this->isSqlModeActive(SqlMode::PipesAsConcat) ? MySQLLexer::CONCAT_PIPES_SYMBOL : MySQLLexer::LOGICAL_OR_OPERATOR; }
void MySQLLexerBase::doIntNumber() { this->type = this->determineNumericType(this->getText()); }
void MySQLLexerBase::doAdddate() { this->type = this->determineFunction(MySQLLexer::ADDDATE_SYMBOL); }
void MySQLLexerBase::doBitAnd() { this->type = this->determineFunction(MySQLLexer::BIT_AND_SYMBOL); }
void MySQLLexerBase::doBitOr() { this->type = this->determineFunction(MySQLLexer::BIT_OR_SYMBOL); }
void MySQLLexerBase::doBitXor() { this->type = this->determineFunction(MySQLLexer::BIT_XOR_SYMBOL); }
void MySQLLexerBase::doCast() { this->type = this->determineFunction(MySQLLexer::CAST_SYMBOL); }
void MySQLLexerBase::doCount() { this->type = this->determineFunction(MySQLLexer::COUNT_SYMBOL); }
void MySQLLexerBase::doCurdate() { this->type = this->determineFunction(MySQLLexer::CURDATE_SYMBOL); }
void MySQLLexerBase::doCurrentDate() { this->type = this->determineFunction(MySQLLexer::CURDATE_SYMBOL); }
void MySQLLexerBase::doCurrentTime() { this->type = this->determineFunction(MySQLLexer::CURTIME_SYMBOL); }
void MySQLLexerBase::doCurtime() { this->type = this->determineFunction(MySQLLexer::CURTIME_SYMBOL); }
void MySQLLexerBase::doDateAdd() { this->type = this->determineFunction(MySQLLexer::DATE_ADD_SYMBOL); }
void MySQLLexerBase::doDateSub() { this->type = this->determineFunction(MySQLLexer::DATE_SUB_SYMBOL); }
void MySQLLexerBase::doExtract() { this->type = this->determineFunction(MySQLLexer::EXTRACT_SYMBOL); }
void MySQLLexerBase::doGroupConcat() { this->type = this->determineFunction(MySQLLexer::GROUP_CONCAT_SYMBOL); }
void MySQLLexerBase::doMax() { this->type = this->determineFunction(MySQLLexer::MAX_SYMBOL); }
void MySQLLexerBase::doMid() { this->type = this->determineFunction(MySQLLexer::SUBSTRING_SYMBOL); }
void MySQLLexerBase::doMin() { this->type = this->determineFunction(MySQLLexer::MIN_SYMBOL); }
void MySQLLexerBase::doNot() { this->type = this->isSqlModeActive(SqlMode::HighNotPrecedence) ? MySQLLexer::NOT2_SYMBOL: MySQLLexer::NOT_SYMBOL; }
void MySQLLexerBase::doNow() { this->type = this->determineFunction(MySQLLexer::NOW_SYMBOL); }
void MySQLLexerBase::doPosition() { this->type = this->determineFunction(MySQLLexer::POSITION_SYMBOL); }
void MySQLLexerBase::doSessionUser() { this->type = this->determineFunction(MySQLLexer::USER_SYMBOL); }
void MySQLLexerBase::doStddevSamp() { this->type = this->determineFunction(MySQLLexer::STDDEV_SAMP_SYMBOL); }
void MySQLLexerBase::doStddev() { this->type = this->determineFunction(MySQLLexer::STD_SYMBOL); }
void MySQLLexerBase::doStddevPop() { this->type = this->determineFunction(MySQLLexer::STD_SYMBOL);}
void MySQLLexerBase::doStd() { this->type = this->determineFunction(MySQLLexer::STD_SYMBOL); }
void MySQLLexerBase::doSubdate() { this->type = this->determineFunction(MySQLLexer::SUBDATE_SYMBOL); }
void MySQLLexerBase::doSubstr() { this->type = this->determineFunction(MySQLLexer::SUBSTRING_SYMBOL); }
void MySQLLexerBase::doSubstring() { this->type = this->determineFunction(MySQLLexer::SUBSTRING_SYMBOL); }
void MySQLLexerBase::doSum() { this->type = this->determineFunction(MySQLLexer::SUM_SYMBOL); }
void MySQLLexerBase::doSysdate() { this->type = this->determineFunction(MySQLLexer::SYSDATE_SYMBOL); }
void MySQLLexerBase::doSystemUser() { this->type = this->determineFunction(MySQLLexer::USER_SYMBOL); }
void MySQLLexerBase::doTrim() { this->type = this->determineFunction(MySQLLexer::TRIM_SYMBOL); }
void MySQLLexerBase::doVariance() { this->type = this->determineFunction(MySQLLexer::VARIANCE_SYMBOL); }
void MySQLLexerBase::doVarPop() { this->type = this->determineFunction(MySQLLexer::VARIANCE_SYMBOL); }
void MySQLLexerBase::doVarSamp() { this->type = this->determineFunction(MySQLLexer::VAR_SAMP_SYMBOL); }
void MySQLLexerBase::doUnderscoreCharset() { this->type = this->checkCharset(this->getText()); }
bool MySQLLexerBase::doDollarQuotedStringText() { return this->serverVersion >= 80034 && this->supportMle; }
bool MySQLLexerBase::isVersionComment() { return inVersionComment; }
bool MySQLLexerBase::isBackTickQuotedId() { return !this->isSqlModeActive(SqlMode::NoBackslashEscapes); }
bool MySQLLexerBase::isDoubleQuotedText() { return !this->isSqlModeActive(SqlMode::NoBackslashEscapes); }
bool MySQLLexerBase::isSingleQuotedText() { return !this->isSqlModeActive(SqlMode::NoBackslashEscapes); }
void MySQLLexerBase::startInVersionComment() { inVersionComment = true; }
void MySQLLexerBase::endInVersionComment() { inVersionComment = false; }
bool MySQLLexerBase::isInVersionComment() { return inVersionComment; }
std::string MySQLLexerBase::longString = "2147483647";
int MySQLLexerBase::longLength = 10;
std::string MySQLLexerBase::signedLongString = "-2147483648";
std::string MySQLLexerBase::longLongString = "9223372036854775807";
int MySQLLexerBase::longLongLength = 19;
std::string MySQLLexerBase::signedLongLongString = "-9223372036854775808";
int MySQLLexerBase::signedLongLongLength = 19;
std::string MySQLLexerBase::unsignedLongLongString = "18446744073709551615";
int MySQLLexerBase::unsignedLongLongLength = 20;
