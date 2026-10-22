/*
PHP grammar.
The MIT License (MIT).
Copyright (c) 2015-2019, Ivan Kochurkin (kvanttt@gmail.com), Positive Technologies.
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

import org.antlr.v4.runtime.*;

public abstract class PhpLexerBase extends Lexer
{
    protected boolean AspTags = true;
    protected boolean _scriptTag;
    protected boolean _styleTag;
    protected String _heredocIdentifier;
    protected int _prevTokenType;
    protected String _htmlNameText;
    protected boolean _phpScript;
    protected boolean _insideString;

    public PhpLexerBase(CharStream input) {
        super(input);
    }

    @Override
    public Token nextToken() {
        CommonToken token = (CommonToken)super.nextToken();

        if (token.getType() == PhpLexer.PHPEnd || token.getType() == PhpLexer.PHPEndSingleLineComment)
        {
            if (_mode == PhpLexer.SingleLineCommentMode)
            {
                // SingleLineCommentMode for such allowed syntax:
                // <?php echo "Hello world"; // comment ?>
                popMode(); // exit from SingleLineComment mode.
            }
            popMode(); // exit from PHP mode.

            if ("</script>".equals(token.getText()))
            {
                _phpScript = false;
                token.setType(PhpLexer.HtmlScriptClose);
            }
            else
            {
                // Add semicolon to the end of statement if it is absente.
                // For example: <?php echo "Hello world" ?>
                if (_prevTokenType == PhpLexer.SemiColon || _prevTokenType == PhpLexer.Colon
                        || _prevTokenType == PhpLexer.OpenCurlyBracket || _prevTokenType == PhpLexer.CloseCurlyBracket)
                {
                    token.setChannel(PhpLexer.SkipChannel);
                }
                else
                {
                    token.setType(PhpLexer.SemiColon);
                }
            }
        }
        else if (token.getType() == PhpLexer.HtmlName)
        {
            _htmlNameText = token.getText();
        }
        else if (token.getType() == PhpLexer.HtmlDoubleQuoteString)
        {
            if ("php".equals(token.getText()) && "language".equals(_htmlNameText))
            {
                _phpScript = true;
            }
        }
        else if (_mode == PhpLexer.HereDoc)
        {
            // Heredoc and Nowdoc syntax support: http://php.net/manual/en/language.types.string.php#language.types.string.syntax.heredoc
            switch (token.getType())
            {
                case PhpLexer.StartHereDoc:
                case PhpLexer.StartNowDoc:
                    _heredocIdentifier = token.getText().substring(3).trim().replace("'","");
                    break;

                case PhpLexer.HereDocText:
                    if (CheckHeredocEnd(token.getText()))
                    {
                        popMode();

                        String heredocIdentifier = GetHeredocIdentifier(token.getText());
                        if (token.getText().trim().endsWith(";"))
                        {
                            token = new CommonToken(PhpLexer.SemiColon, heredocIdentifier + ";\n");
                        }
                        else
                        {
                            token = (CommonToken)super.nextToken();
                            token.setText(heredocIdentifier + "\n;");
                        }
                    }
                    break;
            }
        }
        else if (_mode == PhpLexer.PHP)
        {
            if (_channel != PhpLexer.HIDDEN)
            {
                _prevTokenType = token.getType();
            }
        }

        return token;
    }

    private String GetHeredocIdentifier(String text) {
        String trimmedText = text.trim();
        boolean semi = (trimmedText.length() > 0) ? (trimmedText.charAt(trimmedText.length() - 1) == ';') : false;
        return semi ? trimmedText.substring(0, trimmedText.length() - 1) : trimmedText;
    }

    private boolean CheckHeredocEnd(String text) {
        return GetHeredocIdentifier(text).equals(_heredocIdentifier);
    }

    protected boolean IsNewLineOrStart(int pos) {
        return this._input.LA(pos) <= 0 || this._input.LA(pos) == '\r' || this._input.LA(pos) == '\n';
    }

    protected void PushModeOnHtmlClose() {
        popMode();
        if (_scriptTag)
        {
            if (!_phpScript)
            {
                pushMode(PhpLexer.SCRIPT);
            }
            else
            {
                pushMode(PhpLexer.PHP);
            }
            _scriptTag = false;
        }
        else if (_styleTag)
        {
            pushMode(PhpLexer.STYLE);
            _styleTag = false;
        }
    }

    protected boolean HasAspTags() {
        return this.AspTags;
    }

    protected boolean HasPhpScriptTag() {
        return this._phpScript;
    }

    protected void PopModeOnCurlyBracketClose() {
        if (_insideString)
        {
            _insideString = false;
            setChannel(PhpLexer.SkipChannel);
            popMode();
        }
    }

    protected boolean ShouldPushHereDocMode(int pos) {
        return _input.LA(pos) == '\r' || _input.LA(pos) == '\n';
    }

    protected boolean IsCurlyDollar(int pos) {
        return _input.LA(pos) == '$';
    }

    protected void SetInsideString() {
        _insideString = true;
    }
}