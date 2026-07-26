#include "PythonLexerBase.h"
#include "PythonLexer.h"
#include "PythonParser.h"

// reading the input stream until a return EOF
std::unique_ptr<antlr4::Token> PythonLexerBase::nextToken() {
    this->checkNextToken();
    
    std::unique_ptr<antlr4::Token> next;

    if (!this->pendingTokens.empty())
    {
        next = std::move(*this->pendingTokens.begin()); // add the queued token to the token stream
        this->pendingTokens.erase(this->pendingTokens.begin()); 
    }

    return next;
}

void PythonLexerBase::reset() {
    this->init();
    Lexer::reset();
}

std::unique_ptr<antlr4::Token> PythonLexerBase::cloneToken(
    const std::unique_ptr<antlr4::Token> &source, 
    size_t channel, 
    const std::string &text, 
    size_t type
) {
    return this->_factory->create(
        { this, this->_input }, 
        type,
        text, 
        channel, 
        source->getStartIndex(), 
        source->getStopIndex(), 
        source->getLine(), 
        source->getCharPositionInLine()
    );
}

std::unique_ptr<antlr4::Token> PythonLexerBase::cloneToken(const std::unique_ptr<antlr4::Token> &source, size_t channel) {
    return this->cloneToken(
        source,
        channel,
        source->getText(),
        source->getType()
    );
}

std::unique_ptr<antlr4::Token> PythonLexerBase::cloneToken(const std::unique_ptr<antlr4::Token> &source, const std::string &text) {
    return this->cloneToken(
        source,
        source->getChannel(),
        text,
        source->getType()
    );
}

std::unique_ptr<antlr4::Token> PythonLexerBase::cloneToken(const std::unique_ptr<antlr4::Token>& source) {
    return this->_factory->create(
        { this, this->_input }, 
        source->getType(), 
        source->getText(), 
        source->getChannel(), 
        source->getStartIndex(), 
        source->getStopIndex(), 
        source->getLine(), 
        source->getCharPositionInLine()
    );
}

void PythonLexerBase::init() {
    while (!this->indentLengthStack.empty()) {
        this->indentLengthStack.pop();
    }

    this->pendingTokens.clear();
    this->previousPendingTokenType = 0;
    this->lastPendingTokenTypeFromDefaultChannel = 0;
    this->opened = 0;
    this->paren_or_bracket_openedStack.clear();
    this->braceExpressionStack.clear();
    this->prevBraceExpression = "";
    this->curLexerMode = 0;
    this->lexerModeStack.clear();
    this->wasSpaceIndentation = false;
    this->wasTabIndentation = false;
    this->wasIndentationMixedWithSpacesAndTabs = false;
    this->curToken = nullptr;
    this->ffgToken = nullptr;
}

void PythonLexerBase::checkNextToken() {
    if (this->previousPendingTokenType == antlr4::Token::EOF) {
        return;
    }

    if (this->indentLengthStack.empty()) { // We're at the first token
        this->insertENCODINGtoken();
        this->setCurrentAndFollowingTokens();
        this->handleStartOfInput();
    } else {
        this->setCurrentAndFollowingTokens();
    }


    switch(this->curToken->getType()) {
        case PythonLexer::NEWLINE:
            this->handleNEWLINEtoken();
            break;
        case PythonLexer::LPAR:
        case PythonLexer::LSQB:
        case PythonLexer::LBRACE:
            this->opened++;
            this->addPendingToken(this->curToken);
            break;
        case PythonLexer::RPAR:
        case PythonLexer::RSQB:
        case PythonLexer::RBRACE:
            this->opened--;
            this->addPendingToken(this->curToken);
            break;
        case PythonLexer::FSTRING_MIDDLE: // does not affect the opened field
            this->handleFSTRING_MIDDLEtokenWithDoubleBrace();
            this->addPendingToken(this->curToken);
            break;
        case PythonLexer::COLONEQUAL:
            this->handleCOLONEQUALtokenInFString();
            break;
        case PythonLexer::ERRORTOKEN:
            this->reportLexerError(std::string("token recognition error at: '" + this->curToken->getText() + "'"));
            this->addPendingToken(this->curToken);
            break;
        case PythonLexer::EOF:
            this->handleEOFtoken();
            break;
        default: 
            this->addPendingToken(this->curToken);
            break;
    }

    this->handleFORMAT_SPECIFICATION_MODE();
}

void PythonLexerBase::setCurrentAndFollowingTokens() {
    if (this->ffgToken) {
        this->curToken = this->cloneToken(this->ffgToken);
    } else {
        this->curToken = antlr4::Lexer::nextToken();
    }

    this->checkCurToken(); // ffgToken cannot be used in this method and its sub methods (ffgToken is not yet set)!

    if (this->curToken->getType() == PythonLexer::EOF) {
        this->ffgToken = this->cloneToken(this->curToken);
    } else {
        this->ffgToken = antlr4::Lexer::nextToken();
    }
}

void PythonLexerBase::insertENCODINGtoken() { // https://peps.python.org/pep-0263/
    std::string lineBuilder = "";
    std::string encodingName = "";
    size_t lineCount = 0;
    std::regex ws_commentPattern = std::regex("^[ \t\f]*(#.*)?$");
    auto charStream = this->_input;
    size_t size = charStream->size();
    charStream->seek(0);

    for(size_t i = 0; i < size; i++) {
        auto c = std::to_string(charStream->LA(i + 1)); 
        lineBuilder += c; 

        if (c == "\n" || i == size - 1) {
            auto line = std::regex_replace(lineBuilder, std::regex("\r|\n"), "");
            if (std::regex_match(line, ws_commentPattern)) { // https://peps.python.org/pep-0263/
                encodingName = this->getEncodingName(line);

                if (encodingName != "") {
                    break; // encoding found
                }
            } else {
                break; // statement or backslash found (line is not empty, not whitespace(s), not comment)
            }

            lineCount++;

            if (lineCount >= 2) {
                break; // check only the first two lines
            }

            lineBuilder = "";
        }
    }

    if (encodingName == "") {
        encodingName = "utf-8"; // default Python source code encoding
    }

    std::unique_ptr<antlr4::Token> encodingToken = this->_factory->create(
        {this, this->_input},
        PythonLexer::ENCODING,
        encodingName,
        antlr4::Token::HIDDEN_CHANNEL,
        0,
        0,
        0,
        -1
    );

    this->addPendingToken(encodingToken);
}

std::string PythonLexerBase::getEncodingName(const std::string &commentText) { // https://peps.python.org/pep-0263/#defining-the-encoding
    std::smatch m;
    std::regex encodingCommentPattern("^[ \t\f]*#.*?coding[:=][ \t]*([-_.a-zA-Z0-9]+)");
    if(std::regex_match(commentText, m, encodingCommentPattern))
    {
        return m[1];
    }
    return "";
}

// initialize the indentLengthStack
// hide the leading NEWLINE token(s)
// if exists, find the first statement (not NEWLINE, not EOF token) that comes from the default channel
// insert a leading INDENT token if necessary
void PythonLexerBase::handleStartOfInput() {
    // initialize the stack with a default 0 indentation length
    this->indentLengthStack.push(0); // this will never be popped off

    while (this->curToken->getType() != PythonLexer::EOF) {
        if (this->curToken->getChannel() == antlr4::Token::DEFAULT_CHANNEL) {
            if (this->curToken->getType() == PythonLexer::NEWLINE) {
                // all the NEWLINE tokens must be ignored before the first statement
                this->hideAndAddPendingToken(this->curToken);
            } else { // We're at the first statement
                this->insertLeadingIndentToken();
                return; // continue the processing of the current token with checkNextToken()
            }
        } else {
            this->addPendingToken(this->curToken); // it can be WS, EXPLICIT_LINE_JOINING or COMMENT token
        }
        this->setCurrentAndFollowingTokens();
    } // continue the processing of the EOF token with checkNextToken()
}

void PythonLexerBase::insertLeadingIndentToken() {
    if (this->previousPendingTokenType == PythonLexer::WS) {
        auto prevToken = std::move(*this->pendingTokens.rbegin()); // WS token

        if (this->getIndentationLength(prevToken->getText()) != 0) { // there is an "indentation" before the first statement
            std::string errMsg = "first statement indented";

            this->reportLexerError(errMsg);

            // insert an INDENT token before the first statement to raise an 'unexpected indent' error later by the parser    
            this->createAndAddPendingToken(
                PythonLexer::INDENT, 
                antlr4::Token::DEFAULT_CHANNEL, 
                PythonLexerBase::ERR_TXT + errMsg, 
                this->curToken
            );
        }
    }
}

void PythonLexerBase::handleNEWLINEtoken() {
    if (!this->lexerModeStack.empty()) {
        this->addPendingToken(this->curToken);
    } else if (this->opened > 0) { // We're in an implicit line joining, ignore the current NEWLINE token
        this->hideAndAddPendingToken(this->curToken);
    } else {
        auto nlToken = this->cloneToken(this->curToken); // save the current NEWLINE token
        bool isLookingAhead = this->ffgToken->getType() == PythonLexer::WS;

        if (isLookingAhead) {
            this->setCurrentAndFollowingTokens(); // set the next two tokens
        }

        switch (this->ffgToken->getType()) {
            case PythonLexer::NEWLINE: // We're before a blank line
            case PythonLexer::COMMENT: // We're before a comment
                this->hideAndAddPendingToken(std::move(nlToken));
                if (isLookingAhead) { 
                    this->addPendingToken(this->curToken); // WS token
                }
                break;
            default: 
                this->addPendingToken(std::move(nlToken));
                if (isLookingAhead) { // We're on whitespace(s) followed by a statement
                    auto indentationLength = this->ffgToken->getType() == PythonLexer::EOF ?
                        0 : this->getIndentationLength(this->curToken->getText());
                    
                    if (indentationLength != PythonLexerBase::INVALID_LENGTH) {
                        this->addPendingToken(this->curToken); // WS token
                        this->insertIndentOrDedentToken(indentationLength); // may insert INDENT token or DEDENT token(s)
                    } else {
                        this->reportError("inconsistent use of tabs and spaces in indentation"); 
                    }
                } else { // We're at a newline followed by a statement (there is no whitespace before the statement)
                    this->insertIndentOrDedentToken(0); // may insert DEDENT token(s)
                }
        }
    }
}

void PythonLexerBase::insertIndentOrDedentToken(size_t indentLength) {
    auto prevIndentLength = this->indentLengthStack.top();

    if (indentLength > prevIndentLength) {
        this->createAndAddPendingToken(PythonLexer::INDENT, antlr4::Token::DEFAULT_CHANNEL, this->ffgToken);
        this->indentLengthStack.push(indentLength);
    } else {
        while (indentLength < prevIndentLength) { // more than 1 DEDENT token may be inserted to the token stream
            this->indentLengthStack.pop();
            prevIndentLength = this->indentLengthStack.top();

            if (indentLength <= prevIndentLength) {
                this->createAndAddPendingToken(PythonLexer::DEDENT, antlr4::Token::DEFAULT_CHANNEL, this->ffgToken);
            } else {
                this->reportError("inconsistent dedent");
            }
        }
    }
}

void PythonLexerBase::checkCurToken() {
    switch (this->curToken->getType()) {
        case PythonLexer::FSTRING_START:
            this->setLexerModeByFSTRING_STARTtoken();
            return;
        case PythonLexer::FSTRING_MIDDLE:
            this->handleFSTRING_MIDDLEtokenWithQuoteAndLBrace(); // affect the opened field
            if (this->curToken->getType() == PythonLexer::FSTRING_MIDDLE) {
                return;
            }
            break;
        case PythonLexer::FSTRING_END:
            this->popLexerMode();
            return;
        default:
            if (this->lexerModeStack.empty()) {
                return;
            }
    }

    switch (this->curToken->getType()) { // the following tokens can only come from default mode (after an LBRACE in fstring)
        case PythonLexer::NEWLINE:
            // append the current brace expression with the current newline
            this->appendToBraceExpression(this->curToken->getText());
            this->curToken = this->cloneToken(this->curToken, antlr4::Token::HIDDEN_CHANNEL);
            break;
        case PythonLexer::LBRACE:
            // the outermost brace expression cannot be a dictionary comprehension or a set comprehension
            this->braceExpressionStack.push_back("{");
            this->paren_or_bracket_openedStack.push_back(0);
            this->pushLexerMode(Lexer::DEFAULT_MODE);
            break;
        case PythonLexer::LPAR:
        case PythonLexer::LSQB:
            // append the current brace expression with a "(" or a "["
            this->appendToBraceExpression(this->curToken->getText());
            // https://peps.python.org/pep-0498/#lambdas-inside-expressions
            this->incrementBraceStack();
            break;
        case PythonLexer::RPAR:
        case PythonLexer::RSQB:
            // append the current brace expression with a ")" or a "]"
            this->appendToBraceExpression(this->curToken->getText());
            this->decrementBraceStack();
            break;
        case PythonLexer::COLON:
        case PythonLexer::COLONEQUAL:
            // append the current brace expression with a ":" or a ":="
            this->appendToBraceExpression(this->curToken->getText());
            this->setLexerModeByCOLONorCOLONEQUALtoken();
            break;
        case PythonLexer::RBRACE:
            this->setLexerModeAfterRBRACEtoken();
            break;
        default:
            // append the current brace expression with the current token text
            this->appendToBraceExpression(this->curToken->getText());
    }
}

void PythonLexerBase::appendToBraceExpression(const std::string &text) {
    *this->braceExpressionStack.rbegin() += text;
}

void PythonLexerBase::incrementBraceStack() { // increment the last element (peek() + 1)
    (*this->paren_or_bracket_openedStack.rbegin())++;
}

void PythonLexerBase::decrementBraceStack() { // decrement the last element (peek() - 1)
    (*this->paren_or_bracket_openedStack.rbegin())--;
}

void PythonLexerBase::setLexerModeAfterRBRACEtoken() {
    switch (this->curLexerMode) {
        case Lexer::DEFAULT_MODE:
            this->popLexerMode();
            this->popByBRACE();
            break;
        case PythonLexer::SQ1__FORMAT_SPECIFICATION_MODE:
        case PythonLexer::SQ1R_FORMAT_SPECIFICATION_MODE:
        case PythonLexer::DQ1__FORMAT_SPECIFICATION_MODE:
        case PythonLexer::DQ1R_FORMAT_SPECIFICATION_MODE:
        case PythonLexer::SQ3__FORMAT_SPECIFICATION_MODE:
        case PythonLexer::SQ3R_FORMAT_SPECIFICATION_MODE:
        case PythonLexer::DQ3__FORMAT_SPECIFICATION_MODE:
        case PythonLexer::DQ3R_FORMAT_SPECIFICATION_MODE:
            this->popLexerMode();
            this->popLexerMode();
            this->popByBRACE();
            break;
        default:
            this->reportLexerError("f-string: single '}' is not allowed");
    }
}

void PythonLexerBase::setLexerModeByFSTRING_STARTtoken() {
    std::string curTokenText = this->curToken->getText();
    auto text = curTokenText;
    std::transform(text.cbegin(), text.cend(), text.begin(), [](auto ch) { return std::tolower(ch); });
    std::map<std::string, size_t> modeMap = {
        {"f'", PythonLexer::SQ1__FSTRING_MODE},
        {"rf'", PythonLexer::SQ1R_FSTRING_MODE},
        {"fr'", PythonLexer::SQ1R_FSTRING_MODE},
        {"f\"", PythonLexer::DQ1__FSTRING_MODE},
        {"rf\"", PythonLexer::DQ1R_FSTRING_MODE},
        {"fr\"", PythonLexer::DQ1R_FSTRING_MODE},
        {"f'''", PythonLexer::SQ3__FSTRING_MODE},
        {"rf'''", PythonLexer::SQ3R_FSTRING_MODE},
        {"fr'''", PythonLexer::SQ3R_FSTRING_MODE},
        {"f\"\"\"", PythonLexer::DQ3__FSTRING_MODE},
        {"rf\"\"\"", PythonLexer::DQ3R_FSTRING_MODE},
        {"fr\"\"\"", PythonLexer::DQ3R_FSTRING_MODE},
    };

    if (modeMap.find(text) != modeMap.end()) {
        this->pushLexerMode(modeMap[text]);
    }
}

void PythonLexerBase::setLexerModeByCOLONorCOLONEQUALtoken() {
    if (*this->paren_or_bracket_openedStack.rbegin() == 0) { // stack peek == 0
        auto previousMode = *this->lexerModeStack.rbegin(); // stack peek
        switch (previousMode) { // check the previous lexer mode (the current is DEFAULT_MODE)
            case PythonLexer::SQ1__FSTRING_MODE:
            case PythonLexer::SQ1__FORMAT_SPECIFICATION_MODE:
                this->pushLexerMode(PythonLexer::SQ1__FORMAT_SPECIFICATION_MODE); // continue in format spec. mode
                break;
            case PythonLexer::SQ1R_FSTRING_MODE:
            case PythonLexer::SQ1R_FORMAT_SPECIFICATION_MODE:
                this->pushLexerMode(PythonLexer::SQ1R_FORMAT_SPECIFICATION_MODE); // continue in format spec. mode
                break;
            case PythonLexer::DQ1__FSTRING_MODE:
            case PythonLexer::DQ1__FORMAT_SPECIFICATION_MODE:
                this->pushLexerMode(PythonLexer::DQ1__FORMAT_SPECIFICATION_MODE); // continue in format spec. mode
                break;
            case PythonLexer::DQ1R_FSTRING_MODE:
            case PythonLexer::DQ1R_FORMAT_SPECIFICATION_MODE:
                this->pushLexerMode(PythonLexer::DQ1R_FORMAT_SPECIFICATION_MODE); // continue in format spec. mode
                break;
            case PythonLexer::SQ3__FSTRING_MODE:
            case PythonLexer::SQ3__FORMAT_SPECIFICATION_MODE:
                this->pushLexerMode(PythonLexer::SQ3__FORMAT_SPECIFICATION_MODE); // continue in format spec. mode
                break;
            case PythonLexer::SQ3R_FSTRING_MODE:
            case PythonLexer::SQ3R_FORMAT_SPECIFICATION_MODE:
                this->pushLexerMode(PythonLexer::SQ3R_FORMAT_SPECIFICATION_MODE); // continue in format spec. mode
                break;
            case PythonLexer::DQ3__FSTRING_MODE:
            case PythonLexer::DQ3__FORMAT_SPECIFICATION_MODE:
                this->pushLexerMode(PythonLexer::DQ3__FORMAT_SPECIFICATION_MODE); // continue in format spec. mode
                break;
            case PythonLexer::DQ3R_FSTRING_MODE:
            case PythonLexer::DQ3R_FORMAT_SPECIFICATION_MODE:
                this->pushLexerMode(PythonLexer::DQ3R_FORMAT_SPECIFICATION_MODE); // continue in format spec. mode
                break;
        }
    }
}

void PythonLexerBase::popByBRACE() {
    this->paren_or_bracket_openedStack.pop_back();
    this->prevBraceExpression = *this->braceExpressionStack.rbegin() + "}";
    this->braceExpressionStack.pop_back();

    if (this->braceExpressionStack.size() > 0) {
        // append the current brace expression with the previous brace expression
        (*this->braceExpressionStack.rbegin()) += this->prevBraceExpression;
    }
}

void PythonLexerBase::handleFSTRING_MIDDLEtokenWithDoubleBrace() {
    // Replace the trailing double brace with a single brace and insert a hidden brace token
    auto lastTwoChars = this->getLastTwoCharsOfTheCurTokenText();

    if (lastTwoChars == "{{") {
        this->trimLastCharAddPendingTokenSetCurToken(PythonLexer::LBRACE, "{", antlr4::Token::HIDDEN_CHANNEL);
    } else if (lastTwoChars == "}}") {
        this->trimLastCharAddPendingTokenSetCurToken(PythonLexer::RBRACE, "}", antlr4::Token::HIDDEN_CHANNEL);
    }
}

void PythonLexerBase::handleFSTRING_MIDDLEtokenWithQuoteAndLBrace() {
    // Replace the trailing quote + left_brace with a quote and insert an LBRACE token
    // Replace the trailing backslash + left_brace with a backslash and insert an LBRACE token
    auto lastTwoChars = this->getLastTwoCharsOfTheCurTokenText();

    if (lastTwoChars == "\"{" || lastTwoChars == "'{" || lastTwoChars == "\\{") {
        this->trimLastCharAddPendingTokenSetCurToken(PythonLexer::LBRACE, "{", antlr4::Token::DEFAULT_CHANNEL);
    }
}

std::string PythonLexerBase::getLastTwoCharsOfTheCurTokenText() {
    return this->curToken->getText().substr(-2);
}

void PythonLexerBase::trimLastCharAddPendingTokenSetCurToken(
    size_t number,
    const std::string &text,
    size_t channel
) {
    // Trim the last char and add the modified curToken to the pendingTokens stack
    auto tokenTextWithoutLastChar = this->curToken->getText().substr(0, -1);

    this->addPendingToken(this->cloneToken(this->curToken, tokenTextWithoutLastChar));

    this->createNewCurToken(type, text, channel); // Set curToken
}

void PythonLexerBase::handleCOLONEQUALtokenInFString() {
    if (
        this->lexerModeStack.size() > 0 &&
        *this->paren_or_bracket_openedStack.rbegin() == 0 // stack peek == 0
    ) {
        // In fstring, a colonequal (walrus operator) can only be used in parentheses
        // Not in parentheses, replace COLONEQUAL token with COLON as format specifier
        // and insert the equal symbol to the following FSTRING_MIDDLE token

        this->curToken = this->cloneToken(this->curToken, PythonLexer::COLON, ":", channel);

        if (this->ffgToken->getType() == PythonLexer::FSTRING_MIDDLE) {

            this->ffgToken = this->_factory->create(
                {this, this->_input},
                this->ffgToken->getType(),
                "=" + this->ffgToken->getText(),
                channel, 
                this->ffgToken->getStartIndex() - 1,
                this->ffgToken->getStartIndex(),
                this->ffgToken->getLine(),
                this->ffgToken->getCharPositionInLine() - 1
            );
        } else {
            this->addPendingToken(this->curToken);
            this->createNewCurToken(PythonLexer::FSTRING_MIDDLE, "=", antlr4::Token::DEFAULT_CHANNEL);
        }
    }

    this->addPendingToken(this->curToken);
}

void PythonLexerBase::createNewCurToken(
    size_t type, 
    const std::string &text, 
    size_t channel)
{
    this->curToken = std::move(this->_factory->create(
        {this, this->_input},
        type,
        text,
        channel, 
        this->curToken->getStartIndex() + 1,
        this->curToken->getStartIndex(),
        this->curToken->getLine(),
        this->curToken->getCharPositionInLine()
    ));
}

void PythonLexerBase::pushLexerMode(size_t mode) {
    this->pushMode(mode);
    this->lexerModeStack.push_back(this->curLexerMode);
    this->curLexerMode = mode;
}

void PythonLexerBase::popLexerMode() {
    this->popMode();
    this->curLexerMode = *this->lexerModeStack.rbegin();
    this->lexerModeStack.pop_back();
}

void PythonLexerBase::handleFORMAT_SPECIFICATION_MODE() {
    if (this->lexerModeStack.size() > 0 &&
        this->ffgToken->getType() == PythonLexer::RBRACE) {

        // insert an empty FSTRING_MIDDLE token instead of the missing format specification
        switch (this->curToken->getType()) {
            case PythonLexer::COLON:
                this->createAndAddPendingToken(PythonLexer::FSTRING_MIDDLE, antlr4::Token::DEFAULT_CHANNEL, "", this->ffgToken);
                break;
            case PythonLexer::RBRACE:
                // only if the previous brace expression is not a dictionary comprehension or set comprehension
                if (!this->isDictionaryComprehensionOrSetComprehension(this->prevBraceExpression)) {
                    this->createAndAddPendingToken(PythonLexer::FSTRING_MIDDLE, antlr4::Token::DEFAULT_CHANNEL, "", this->ffgToken);
                }
                break;
        }
    }
}

bool PythonLexerBase::isDictionaryComprehensionOrSetComprehension(const std::string &code) {
    auto inputStream = std::make_unique<antlr4::ANTLRInputStream>(code);
    auto lexer = std::make_unique<PythonLexer>(inputStream.get());
    auto tokenStream = std::make_unique<antlr4::CommonTokenStream>(lexer.get());
    auto parser = std::make_unique<PythonParser>(tokenStream.get());

    // Disable error listeners to suppress console output
    lexer->removeErrorListeners();
    parser->removeErrorListeners();

    parser->dictcomp(); // Try parsing as dictionary comprehension
    if (parser->getNumberOfSyntaxErrors() == 0)
        return true;

    parser = std::make_unique<PythonParser>(tokenStream.get());

    tokenStream->seek(0);
    
    parser->removeErrorListeners();
    parser->setcomp(); // Try parsing as set comprehension
    return parser->getNumberOfSyntaxErrors() == 0;
}

void PythonLexerBase::insertTrailingTokens() {
    switch (this->lastPendingTokenTypeFromDefaultChannel) {
        case PythonLexer::NEWLINE:
        case PythonLexer::DEDENT:
            break; // no trailing NEWLINE token is needed
        default:
            // insert an extra trailing NEWLINE token that serves as the end of the last statement
            this->createAndAddPendingToken(PythonLexer::NEWLINE, antlr4::Token::DEFAULT_CHANNEL, this->ffgToken); // ffgToken is EOF
            break;
    }
    this->insertIndentOrDedentToken(0); // Now insert as much trailing DEDENT tokens as needed
}

void PythonLexerBase::handleEOFtoken() {
    if (this->lastPendingTokenTypeFromDefaultChannel > 0) {
        // there was a statement in the input (leading NEWLINE tokens are hidden)
        this->insertTrailingTokens();
    }
    this->addPendingToken(this->curToken);
}

void PythonLexerBase::hideAndAddPendingToken(const std::unique_ptr<antlr4::Token> &tkn) {
    this->addPendingToken(this->cloneToken(tkn, antlr4::Token::HIDDEN_CHANNEL));
}

void PythonLexerBase::createAndAddPendingToken(
    size_t type, 
    size_t channel, 
    const std::string &text, 
    const std::unique_ptr<antlr4::Token> &sampleToken
) {
    this->addPendingToken(
        this->_factory->create(
            {this, this->_input},
            type,
            text,
            channel,
            sampleToken->getStartIndex(),
            sampleToken->getStartIndex() - 1,
            sampleToken->getLine(),
            sampleToken->getCharPositionInLine()
        )
    );
}

void PythonLexerBase::createAndAddPendingToken(
    size_t type, 
    size_t channel, 
    const std::unique_ptr<antlr4::Token> &sampleToken
) {
    this->createAndAddPendingToken(
        type, 
        channel, 
        "<$" + this->getVocabulary().getDisplayName(type) + ">",
        sampleToken
    );
}

void PythonLexerBase::addPendingToken(const std::unique_ptr<antlr4::Token> &tkn) {
    // save the last pending token type because the pendingTokens list can be empty by the nextToken()
    this->previousPendingTokenType = tkn->getType();
    if (tkn->getChannel() == antlr4::Token::DEFAULT_CHANNEL) {
        this->lastPendingTokenTypeFromDefaultChannel = this->previousPendingTokenType;
    }

    this->pendingTokens.push_back(this->cloneToken(tkn)) /* .addLast(token) */;
}

size_t PythonLexerBase::getIndentationLength(const std::string &indentText) { // the indentText may contain spaces, tabs or form feeds
    const size_t TAB_LENGTH = 8; // the standard number of spaces to replace a tab to spaces
    size_t length = 0;
    for (const auto &ch : indentText) {
        switch (ch) {
            case ' ':
                this->wasSpaceIndentation = true;
                length += 1;
                break;
            case '\t':
                this->wasTabIndentation = true;
                length += TAB_LENGTH - (length % TAB_LENGTH);
                break;
            case '\f': // form feed
                length = 0;
                break;
        }
    }

    if (this->wasTabIndentation && this->wasSpaceIndentation) {
        if (!this->wasIndentationMixedWithSpacesAndTabs) {
            this->wasIndentationMixedWithSpacesAndTabs = true;
            length = PythonLexerBase::INVALID_LENGTH; // only for the first inconsistent indent
        }
    }

    return length;
}

void PythonLexerBase::reportLexerError(const std::string &errMsg) {
    this->getErrorListenerDispatch().syntaxError(
        this, 
        0 /* this->curToken */, 
        this->curToken->getLine(), 
        this->curToken->getCharPositionInLine(), 
        " LEXER" + PythonLexerBase::ERR_TXT + errMsg, 
        nullptr
    );
}

void PythonLexerBase::reportError(const std::string &errMsg) {
    this->reportLexerError(errMsg);

    // the ERRORTOKEN will raise an error in the parser
    this->createAndAddPendingToken(PythonLexer::ERRORTOKEN, antlr4::Token::DEFAULT_CHANNEL, PythonLexerBase::ERR_TXT + errMsg, this->ffgToken);
}
