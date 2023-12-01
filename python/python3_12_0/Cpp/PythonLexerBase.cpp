#include "PythonLexer.h"
#include "PythonParser.h"
#include <regex>

using namespace antlr4;

PythonLexerBase::PythonLexerBase(antlr4::CharStream *input): Lexer(input)
{
}

std::unique_ptr<antlr4::Token> PythonLexerBase::nextToken()
{
	CheckNextToken();
	std::unique_ptr<antlr4::Token> firstPendingToken = std::move(*_pendingTokens.begin());
	_pendingTokens.erase(_pendingTokens.begin());
	return firstPendingToken; // add the queued token to the token stream
}

void PythonLexerBase::CheckNextToken()
{
	if (_previousPendingTokenType != antlr4::Token::EOF)
	{
		SetCurrentAndFollowingTokens();
		if (_indentLengths.size() == 0) // We're at the tfirst token
		{
			HandleStartOfInput();
		}

		switch (_curToken->getType())
		{
			case PythonLexer::LPAR:
			case PythonLexer::LSQB:
			case PythonLexer::LBRACE:
				_opened++;
				AddPendingToken(std::move(_curToken));
				break;
			case PythonLexer::RPAR:
			case PythonLexer::RSQB:
			case PythonLexer::RBRACE:
				_opened--;
				AddPendingToken(std::move(_curToken));
				break;
			case PythonLexer::NEWLINE:
				HandleNEWLINEtoken();
				break;
			case PythonLexer::STRING:
				HandleSTRINGtoken();
				break;
			case PythonLexer::FSTRING_MIDDLE:
				HandleFSTRING_MIDDLE_token();
				break;
			case PythonLexer::ERROR_TOKEN:
				ReportLexerError("token recognition error at: '" + _curToken->getText() + "'");
				AddPendingToken(std::move(_curToken));
				break;
			case antlr4::Token::EOF:
				HandleEOFtoken();
				break;
			default:
				AddPendingToken(std::move(_curToken));
				break;
		}
		HandleFORMAT_SPECIFICATION_MODE();
	}
}
		
void PythonLexerBase::SetCurrentAndFollowingTokens()
{
	_curToken = _ffgToken == nullptr ?
		    new CommonToken(Lexer::nextToken()) :
		    new CommonToken(_ffgToken);

	HandleFStringLexerModes();

	_ffgToken = _curToken->getType() == antlr4::Token::EOF ?
		_curToken :
		Lexer::nextToken();
}

void PythonLexerBase::HandleStartOfInput()
{
    // initialize the stack with a default 0 indentation length
    _indentLengths.AddLast(0); // this will never be popped off
    while (_curToken->getType() != antlr4::Token::EOF)
    {
        if (_curToken->getChannel() == DEFAULT_TOKEN_CHANNEL)
        {
            if (_curToken->getType() == PythonLexer::NEWLINE)
            {
                // all the NEWLINE tokens must be ignored before the first statement
                HideAndAddPendingToken(std::move(_curToken));
            }
            else
            { // We're at the first statement
                InsertLeadingIndentToken();
                return; // continue the processing of the current token with CheckNextToken()
            }
        }
        else
        {
            AddPendingToken(std::move(_curToken)); // it can be WS, EXPLICIT_LINE_JOINING, or COMMENT token
        }
        SetCurrentAndFollowingTokens();
    } // continue the processing of the EOF token with CheckNextToken()
}


void PythonLexerBase::HandleNEWLINEtoken()
{
    if (_opened > 0)
    {
        // We're in an implicit line joining, ignore the current NEWLINE token
        HideAndAddPendingToken(std::move(_curToken));
    }
    else
    {
        std::unique_ptr<CommonToken> nlToken = std::move(_curToken); // save the current NEWLINE token
        bool isLookingAhead = _ffgToken->getType() == PythonLexer::WS;
        if (isLookingAhead)
        {
            SetCurrentAndFollowingTokens(); // set the two next tokens
        }

        switch (_ffgToken->getType())
        {
        case PythonLexer::NEWLINE:      // We're before a blank line
        case PythonLexer::COMMENT:      // We're before a comment
        case PythonLexer::TYPE_COMMENT: // We're before a type comment
            HideAndAddPendingToken(std::move(nlToken));
            if (isLookingAhead)
            {
                AddPendingToken(std::move(_curToken));  // WS token
            }
            break;
        default:
            AddPendingToken(std::move(nlToken));
            if (isLookingAhead)
            { // We're on whitespace(s) followed by a statement
                int indentationLength = _ffgToken.Type == Eof ?
                    0 :
                    GetIndentationLength(_curToken.Text);

                if (indentationLength != INVALID_LENGTH)
                {
                    AddPendingToken(_curToken);  // WS token
                    InsertIndentOrDedentToken(indentationLength); // may insert INDENT token or DEDENT token(s)                            
                }
                else
                {
                    ReportError("inconsistent use of tabs and spaces in indentation");
                }
            }
            else
            {
                // We're at a newline followed by a statement (there is no whitespace before the statement)
                InsertIndentOrDedentToken(0); // may insert DEDENT token(s)
            }
            break;
        }
    }
}







std::unique_ptr<antlr4::Token> PythonLexerBase::createDedent() {
    std::unique_ptr<antlr4::CommonToken> dedent = commonToken(PythonParser::DEDENT, "");
    return dedent;
}

std::unique_ptr<antlr4::CommonToken> PythonLexerBase::commonToken(size_t type, const std::string& text) {
    int stop = getCharIndex() - 1;
    int start = text.empty() ? stop : stop - text.size() + 1;
    return _factory->create({ this, _input }, type, text, DEFAULT_TOKEN_CHANNEL, start, stop, lastToken ? lastToken->getLine() : 0, lastToken ? lastToken->getCharPositionInLine() : 0);
}

std::unique_ptr<antlr4::CommonToken> PythonLexerBase::cloneToken(const std::unique_ptr<antlr4::Token>& source) {
    return _factory->create({ this, _input }, source->getType(), source->getText(), source->getChannel(), source->getStartIndex(), source->getStopIndex(), source->getLine(), source->getCharPositionInLine());
}

int PythonLexerBase::getIndentationCount(const std::string& spaces) {
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

bool PythonLexerBase::atStartOfInput() {
    return getCharPositionInLine() == 0 && getLine() == 1;
}

void PythonLexerBase::openBrace() {
    this->opened++;
}

void PythonLexerBase::closeBrace() {
    this->opened--;
}


void PythonLexerBase::onNewLine()
{
    std::string newLine = std::regex_replace(getText(), std::regex("[^\r\n\f]+"), "");
    std::string spaces = std::regex_replace(getText(), std::regex("[\r\n\f]+"), "");
    int next = _input->LA(1);
    int nextnext = _input->LA(2);
    if (opened > 0 || (nextnext != -1 && (next == '\r' || next == '\n' || next == '\f' || next == '#'))) {
        skip();
    }
    else {
        emit(commonToken(PythonLexer::NEWLINE, newLine));
        int indent = getIndentationCount(spaces);
        int previous = indents.empty() ? 0 : indents.top();
        if (indent == previous) {
            skip();
        }
        else if (indent > previous) {
            indents.push(indent);
            emit(commonToken(PythonLexer::INDENT, spaces));
        }
        else {
            while(!indents.empty() && indents.top() > indent) {
                emit(createDedent());
                indents.pop();
            }
        }
    }
}


void PythonLexerBase::reset()
{
    tokens = std::vector<std::unique_ptr<antlr4::Token>>{};
    indents = std::stack<int>{};
    opened = 0;
    lastToken = nullptr;
    Lexer::reset();
}

void PythonLexerBase::AddPendingToken(std::unique_ptr<antlr4::Token> token)
{
    // save the last pending token type because the _pendingTokens linked list can be empty by the nextToken()
    _previousPendingTokenType = token.Type;
    if (token.Channel == TokenConstants.DefaultChannel)
    {
        _lastPendingTokenTypeForDefaultChannel = _previousPendingTokenType;
    }
    _pendingTokens.AddLast(token);
}

void PythonLexerBase::ReportLexerError(std::string errMsg)
{
    ErrorListenerDispatch.SyntaxError(ErrorOutput, this, _curToken.Type, _curToken.Line, _curToken.Column, _ERR_TXT + errMsg, null);
}

