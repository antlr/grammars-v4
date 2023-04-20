#pragma once

#include "antlr4-runtime.h"

#include "ystd.hpp"

class PhpLexerBase : public antlr4::Lexer
{
protected:
	bool AspTags = true;
	bool _scriptTag = false;
	bool _styleTag = false;
	std::string _heredocIdentifier;
	int _prevTokenType = 0;
	std::string _htmlNameText;
	bool _phpScript = false;
	bool _insideString = false;

	PhpLexerBase(antlr4::CharStream* input)
		: antlr4::Lexer(input)
	{
	}

public:
	std::unique_ptr<antlr4::Token> nextToken() override;

protected:
	std::string GetHeredocIdentifier(std::string text)
	{
		ystd::trim(text);
		bool semi = text.length() > 0 ? text[text.length() - 1] == ';' : false;
		if (semi) text.resize(text.length() - 1);
		return std::move(text);
	}

	bool CheckHeredocEnd(std::string text)
	{
		return GetHeredocIdentifier(std::move(text)) == _heredocIdentifier;
	}

	bool IsNewLineOrStart(int pos)
	{
		return _input->LA(pos) <= 0 || _input->LA(pos) == '\r' || _input->LA(pos) == '\n';
	}

	void PushModeOnHtmlClose();

	bool HasAspTags()
	{
		return AspTags;
	}

	bool HasPhpScriptTag()
	{
		return _phpScript;
	}

	void PopModeOnCurlyBracketClose();

	bool ShouldPushHereDocMode(int pos)
	{
		return _input->LA(pos) == '\r' || _input->LA(pos) == '\n';
	}

	bool IsCurlyDollar(int pos) {
		return _input->LA(pos) == '$';
	}

	void SetInsideString() {
		_insideString = true;
	}
};
