#include "PythonLexer.h"

using namespace antlr4;

PythonLexerBase::PythonLexerBase(antlr4::CharStream *input) : Lexer(input)
{
	TabSize = 8;
	_number_of_elements = 32;
	_buffer = new std::unique_ptr<antlr4::Token>[_number_of_elements];
	for (int i = 0; i < 32; ++i) _buffer[i] = nullptr;
	_lastTokenInd = 0;
	_firstTokensInd = 0;
	_lastTokenNull = true;
	_opened = 0;
}

void PythonLexerBase::emit(std::unique_ptr<antlr4::Token> token)
{
	auto copy = new CommonToken((Token*) token.get());
	Lexer::emit(std::move(token));
	if (!(_buffer[_firstTokensInd] == nullptr))
	{
		IncTokenInd(_lastTokenInd);
		if (_lastTokenInd == _firstTokensInd)
		{
			// Enlarge buffer
			int new_number_of_elements = _number_of_elements * 2;
			std::unique_ptr<antlr4::Token>* newArray = new std::unique_ptr<antlr4::Token>[new_number_of_elements];
			int destInd = new_number_of_elements - (_number_of_elements - _firstTokensInd);
			for (int i = 0; i < _firstTokensInd; ++i) newArray[i] = std::move(_buffer[i]);
			for (int i = 0; i < _number_of_elements - _firstTokensInd; ++i) newArray[destInd + i] = std::move(_buffer[_firstTokensInd + i]);
			_number_of_elements = new_number_of_elements;
			_firstTokensInd = destInd;
			_buffer = newArray;
		}
	}
	_lastTokenNull = copy == nullptr;
	_lastTokenType = copy != nullptr ? copy->getType() : 0;
	_buffer[_lastTokenInd] = std::make_unique<CommonToken>(copy);
}

std::unique_ptr<antlr4::Token> PythonLexerBase::nextToken()
{
	// Check if the end-of-file is ahead and there are still some DEDENTS expected.
	if (_input->LA(1) == antlr4::Token::EOF && _indents.size() > 0)
	{
		if (_buffer[_lastTokenInd] == nullptr || _buffer[_lastTokenInd]->getType() != PythonLexer::LINE_BREAK)
		{
			// First emit an extra line break that serves as the end of the statement.
			Emit(PythonLexer::LINE_BREAK);
		}

		// Now emit as much DEDENT tokens as needed.
		while (_indents.size() != 0)
		{
			Emit(PythonLexer::DEDENT);
			_indents.pop();
		}
	}

	std::unique_ptr<antlr4::Token> next = std::move(Lexer::nextToken());

	if (_buffer[_firstTokensInd] == nullptr)
	{
		return next;
	}

	auto result = std::move(_buffer[_firstTokensInd]);
	_buffer[_firstTokensInd] = nullptr;

	if (_firstTokensInd != _lastTokenInd)
	{
		IncTokenInd(_firstTokensInd);
	}
	return result;
}

void PythonLexerBase::HandleNewLine()
{
	Emit(PythonLexer::NEWLINE, PythonLexer::HIDDEN, getText());

	char next = (char) _input->LA(1);

	// Process whitespaces in HandleSpaces
	if (next != ' ' && next != '\t' && IsNotNewLineOrComment(next))
	{
		ProcessNewLine(0);
	}
}

void PythonLexerBase::HandleSpaces()
{
	char next = (char) _input->LA(1);

	if ((_lastTokenNull || _lastTokenType == PythonLexer::NEWLINE) && IsNotNewLineOrComment(next))
	{
		// Calculates the indentation of the provided spaces, taking the
		// following rules into account:
		//
		// "Tabs are replaced (from left to right) by one to eight spaces
		//  such that the total number of characters up to and including
		//  the replacement is a multiple of eight [...]"
		//
		//  -- https://docs.python.org/3.1/reference/lexical_analysis.html#indentation

		int indent = 0;
		auto text = getText();
		int length = text.size();
		for (int i = 0; i < length; i++)
		{
			char c = text[i];
			indent += c == '\t' ? TabSize - indent % TabSize : 1;
		}

		ProcessNewLine(indent);
	}

	Emit(PythonLexer::WS, PythonLexer::HIDDEN, getText());
}

void PythonLexerBase::IncIndentLevel()
{
	_opened++;
}

void PythonLexerBase::DecIndentLevel()
{
	if (_opened > 0)
	{
		--_opened;
	}
}

bool PythonLexerBase::IsNotNewLineOrComment(char next)
{
	return _opened == 0 && next != '\r' && next != '\n' && next != '\f' && next != '#';
}

void PythonLexerBase::ProcessNewLine(int indent)
{
	Emit(PythonLexer::LINE_BREAK);

	int previous = _indents.size() == 0 ? 0 : _indents.top();

	if (indent > previous)
	{
		_indents.push(indent);
		Emit(PythonLexer::INDENT);
	}
	else
	{
	    // Possibly emit more than 1 DEDENT token.
		while (_indents.size() != 0 && _indents.top() > indent)
		{
			Emit(PythonLexer::DEDENT);
			_indents.pop();
		}
	}
}

void PythonLexerBase::IncTokenInd(int& ind)
{
	ind = (ind + 1) % _number_of_elements;
}

void PythonLexerBase::Emit(int tokenType, int channel, std::string text)
{
	std::unique_ptr<antlr4::Token> token(
		_factory->create({ this, _input }, //{ this, this->getInputStream() },
				 tokenType, text, channel, getCharIndex() - text.size(), getCharIndex() - 1,
				getLine(), getCharPositionInLine()));
//	token->setLine(getLine());
//	token->setColumn(getColumn());
//	token->setText(text);
	emit(std::move(token));
}
