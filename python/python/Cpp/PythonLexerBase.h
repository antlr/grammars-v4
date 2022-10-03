#pragma once

#include <stack>

#include "antlr4-runtime.h"

class PythonLexerBase : public antlr4::Lexer {
	public:
		PythonLexerBase(antlr4::CharStream *input);
		int TabSize;
		int _opened;
		std::stack<int> _indents;
		int _firstTokensInd;
		int _lastTokenInd;
		std::unique_ptr<antlr4::Token>* _buffer;
		bool _lastTokenNull;
		int _lastTokenType;
		int _number_of_elements;
		
		void emit(std::unique_ptr<antlr4::Token> newToken);
		std::unique_ptr<antlr4::Token> nextToken();
		void HandleNewLine();
		void HandleSpaces();
		void IncIndentLevel();
		void DecIndentLevel();
		bool IsNotNewLineOrComment(char next);
		void ProcessNewLine(int indent);
		void IncTokenInd(int& ind);
		void Emit(int tokenType, int channel = 0, std::string text = "");
};
