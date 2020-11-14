/*
PHP grammar.
The MIT License (MIT).
Copyright (c) 2015-2017, Ivan Kochurkin (kvanttt@gmail.com), Positive Technologies.
Copyright (c) 2019, Thierry Marianne (thierry.marianne@weaving-the-web.org)

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in
all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
THE SOFTWARE.
*/

using Antlr4.Runtime;
// Replace PhpParser with its fully qualified name below
//using static PhpLexer;

public abstract class PhpBaseLexer : Lexer
{
    protected bool AspTags = true;
    protected bool _scriptTag;
    protected bool _styleTag;
    protected string _heredocIdentifier;
    protected int _prevTokenType;
    protected string _htmlNameText;
    protected bool _phpScript;
    protected bool _insideString;

    public PhpBaseLexer(ICharStream input)
        : base(input)
    {
    }

    public override IToken NextToken()
    {
        CommonToken token = (CommonToken)base.NextToken();

        if (token.Type == PHPEnd || token.Type == PHPEndSingleLineComment)
        {
            if (_mode == SingleLineCommentMode)
            {
                // SingleLineCommentMode for such allowed syntax:
                // <?php echo "Hello world"; // comment ?>
                PopMode(); // exit from SingleLineComment mode.
            }
            PopMode(); // exit from PHP mode.

            if (string.Equals(token.Text, "</script>", System.StringComparison.Ordinal))
            {
                _phpScript = false;
                token.Type = ScriptClose;
            }
            else
            {
                // Add semicolon to the end of statement if it is absente.
                // For example: <?php echo "Hello world" ?>
                if (_prevTokenType == SemiColon || _prevTokenType == Colon
                    || _prevTokenType == OpenCurlyBracket || _prevTokenType == CloseCurlyBracket)
                {
                    token.Channel = SkipChannel;
                }
                else
                {
                    token.Type = SemiColon;
                }
            }
        }
        else if (token.Type == HtmlName)
        {
            _htmlNameText = token.Text;
        }
        else if (token.Type == HtmlDoubleQuoteString)
        {
            if (string.Equals(token.Text, "php", System.StringComparison.OrdinalIgnoreCase) &&
                string.Equals(_htmlNameText, "language"))
            {
                _phpScript = true;
            }
        }
        else if (_mode == HereDoc)
        {
            // Heredoc and Nowdoc syntax support: http://php.net/manual/en/language.types.string.php#language.types.string.syntax.heredoc
            switch (token.Type)
            {
                case StartHereDoc:
                case StartNowDoc:
                    _heredocIdentifier = token.Text.Substring(3).Trim().Trim('\'');
                    break;

                case HereDocText:
                    if (CheckHeredocEnd(token.Text))
                    {
                        PopMode();

                        var heredocIdentifier = GetHeredocIdentifier(token.Text);
                        if (token.Text.Trim().EndsWith(";"))
                        {
                            token.Text = heredocIdentifier + ";\n";
                            token.Type = SemiColon;
                        }
                        else
                        {
                            token = (CommonToken)base.NextToken();
                            token.Text = heredocIdentifier + "\n;";
                        }
                    }
                    break;
            }
        }
        else if (_mode == PHP)
        {
            if (_channel != Hidden)
            {
                _prevTokenType = token.Type;
            }
        }

        return token;
    }

    protected string GetHeredocIdentifier(string text)
    {
        text = text.Trim();
        bool semi = text.Length > 0 ? text[text.Length - 1] == ';' : false;
        return semi ? text.Substring(0, text.Length - 1) : text;
    }

    protected bool CheckHeredocEnd(string text)
    {
        return string.Equals(GetHeredocIdentifier(text), _heredocIdentifier, System.StringComparison.Ordinal);
    }

    protected bool IsNewLineOrStart(int pos)
    {
        return _input.La(pos) <= 0 || _input.La(pos) == '\r' || _input.La(pos) == '\n';
    }

    protected void PushModeOnHtmlClose()
    {
        PopMode();
        if (_scriptTag)
        {
            if (!_phpScript)
            {
                PushMode(SCRIPT);
            }
            else
            {
                PushMode(PHP);
            }
            _scriptTag = false;
        }
        else if (_styleTag)
        {
            PushMode(STYLE);
            _styleTag = false;
        }
    }

    protected bool HasAspTags()
    {
        return AspTags;
    }

    protected bool HasPhpScriptTag()
    {
        return _phpScript;
    }

    protected void PopModeOnCurlyBracketClose()
    {
        if (_insideString)
        {
            _insideString = false;
            Channel = SkipChannel;
            PopMode();
        }
    }

    protected bool ShouldPushHereDocMode(int pos)
    {
        return _input.La(pos) == '\r' || _input.La(pos) == '\n';
    }

    protected bool IsCurlyDollar(int pos) {
        return _input.La(pos) == '$';
    }

    protected void SetInsideString() {
        _insideString = true;
    }
}