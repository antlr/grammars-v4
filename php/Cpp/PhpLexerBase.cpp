
#include "PhpLexerBase.h"
#include "PhpLexer.h"
#include "ystd.hpp"

using namespace antlr4;

using namespace std::string_view_literals;

std::unique_ptr<antlr4::Token> PhpLexerBase::nextToken()
{
    std::unique_ptr<antlr4::CommonToken> token = ystd::static_pointer_cast<antlr4::CommonToken>(antlr4::Lexer::nextToken());

    auto tokenType = token->getType();
    if (tokenType == PhpLexer::PHPEnd || tokenType == PhpLexer::PHPEndSingleLineComment)
    {
        if (this->mode == PhpLexer::SingleLineCommentMode)
        {
            // SingleLineCommentMode for such allowed syntax:
            // <?php echo "Hello world"; // comment ?>
            popMode(); // exit from SingleLineComment mode.
        }
        popMode(); // exit from PHP mode.

        if (token->getText() == "</script>"sv)
        {
            _phpScript = false;
            token->setType(PhpLexer::HtmlScriptClose);
        }
        else
        {
            // Add semicolon to the end of statement if it is absente.
            // For example: <?php echo "Hello world" ?>
            if (_prevTokenType == PhpLexer::SemiColon || _prevTokenType == PhpLexer::Colon
                || _prevTokenType == PhpLexer::OpenCurlyBracket || _prevTokenType == PhpLexer::CloseCurlyBracket)
            {
                token->setChannel(PhpLexer::SkipChannel);
            }
            else
            {
                token->setType(PhpLexer::SemiColon);
            }
        }
    }
    else if (tokenType == PhpLexer::HtmlName)
    {
        _htmlNameText = token->getText();
    }
    else if (tokenType == PhpLexer::HtmlDoubleQuoteString)
    {
        if (ystd::ic::iequals(token->getText(), "php"sv) &&
            _htmlNameText == "language"sv)
        {
            _phpScript = true;
        }
    }
    else if (this->mode == PhpLexer::HereDoc)
    {
        // Heredoc and Nowdoc syntax support: http://php.net/manual/en/language.types.string.php#language.types.string.syntax.heredoc
        switch (tokenType)
        {
        case PhpLexer::StartHereDoc:
        case PhpLexer::StartNowDoc:
            (void)0;
            {
                auto text = token->getText();
                text.erase(0, 3);
                _heredocIdentifier = std::move(ystd::replace(ystd::trim(text), "\'"sv, ""sv)); // token.Text.Substring(3).Trim().Trim('\'');
                break;
            }

        case PhpLexer::HereDocText:
            if (CheckHeredocEnd(token->getText()))
            {
                popMode();
                auto text = token->getText();
                ystd::trim(text);
                auto heredocIdentifier = GetHeredocIdentifier(text);
                if (ystd::ends_with(text, ";"sv))
                {
                    token->setText(heredocIdentifier += ";\n"sv);
                    token->setType(PhpLexer::SemiColon);
                }
                else
                {
                    token = ystd::static_pointer_cast<antlr4::CommonToken>(antlr4::Lexer::nextToken());
                    token->setText(heredocIdentifier += "\n;"sv);
                }
            }
            break;
        }
    }
    else if (this->mode == PhpLexer::PHP)
    {
        if (this->channel != HIDDEN)
        {
            _prevTokenType = tokenType;
        }
    }

    return token;
}

void PhpLexerBase::PushModeOnHtmlClose()
{
    popMode();
    if (_scriptTag)
    {
        if (!_phpScript)
        {
            pushMode(PhpLexer::SCRIPT);
        }
        else
        {
            pushMode(PhpLexer::PHP);
        }
        _scriptTag = false;
    }
    else if (_styleTag)
    {
        pushMode(PhpLexer::STYLE);
        _styleTag = false;
    }
}

void PhpLexerBase::PopModeOnCurlyBracketClose()
{
    if (_insideString)
    {
        _insideString = false;
        this->channel = PhpLexer::SkipChannel;
        popMode();
    }
}
