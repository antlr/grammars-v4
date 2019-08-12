import org.antlr.v4.runtime.*;

public abstract class PhpBaseLexer extends Lexer
{
    public boolean AspTags = true;
    boolean _scriptTag;
    boolean _styleTag;
    String _heredocIdentifier;
    int _prevTokenType;
    String _htmlNameText;
    boolean _phpScript;
    boolean _insideString;

    @Override
    public Token nextToken()
    {
        CommonToken token = (CommonToken)super.nextToken();

        if (token.getType() == PHPEnd || token.getType() == PHPEndSingleLineComment)
        {
            if (_mode == SingleLineCommentMode)
            {
                // SingleLineCommentMode for such allowed syntax:
                // <?php echo "Hello world"; // comment ?>
                popMode(); // exit from SingleLineComment mode.
            }
            popMode(); // exit from PHP mode.

            if (token.getText().equals("</script>"))
            {
                _phpScript = false;
                token.setType(ScriptClose);
            }
            else
            {
                // Add semicolon to the end of statement if it is absente.
                // For example: <?php echo "Hello world" ?>
                if (_prevTokenType == SemiColon || _prevTokenType == Colon
                        || _prevTokenType == OpenCurlyBracket || _prevTokenType == CloseCurlyBracket)
                {
                    token.setChannel(SkipChannel);
                }
                else
                {
                    token = new CommonToken(SemiColon);
                }
            }
        }
        else if (token.getType() == HtmlName)
        {
            _htmlNameText = token.getText();
        }
        else if (token.getType() == HtmlDoubleQuoteString)
        {
            if (token.getText().equals("php") && _htmlNameText.equals("language"))
            {
                _phpScript = true;
            }
        }
        else if (_mode == HereDoc)
        {
            // Heredoc and Nowdoc syntax support: http://php.net/manual/en/language.types.string.php#language.types.string.syntax.heredoc
            switch (token.getType())
            {
                case StartHereDoc:
                case StartNowDoc:
                    _heredocIdentifier = token.getText().substring(3).trim().replace("\'","");
                    break;

                case HereDocText:
                    if (CheckHeredocEnd(token.getText()))
                    {
                        popMode();

                        String heredocIdentifier = GetHeredocIdentifier(token.getText());
                        if (token.getText().trim().endsWith(";"))
                        {
                            token = new CommonToken(SemiColon, heredocIdentifier + ";\n");
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
        else if (_mode == PHP)
        {
            if (_channel != HIDDEN)
            {
                _prevTokenType = token.getType();
            }
        }

        return token;
    }

    private String GetHeredocIdentifier(String text)
    {
        String trimmedText = text.trim();
        boolean semi = (trimmedText.length() > 0) ? (trimmedText.charAt(trimmedText.length() - 1) == ';') : false;
        return semi ? trimmedText.substring(0, trimmedText.length() - 1) : trimmedText;
    }

    private boolean CheckHeredocEnd(String text)
    {
        return GetHeredocIdentifier(text).equals(_heredocIdentifier);
    }

    protected boolean IsNewLineOrStart(int pos)
    {
        return this._input.LA(pos) <= 0 || this._input.LA(pos) == '\r' || this._input.LA(pos) == '\n';
    }

    protected void PushModeOnHtmlClose() {
        popMode();
        if (_scriptTag)
        {
            if (!_phpScript)
            {
                pushMode(SCRIPT);
            }
            else
            {
                pushMode(PHP);
            }
            _scriptTag = false;
        }
        else if (_styleTag)
        {
            pushMode(STYLE);
            _styleTag = false;
        }
    }

    protected boolean HasAspTags()
    {
        return this.AspTags;
    }

    protected boolean HasPhpScriptTag()
    {
        return this._phpScript;
    }

    protected void PopModeOnCurlyBracketClose()
    {
        if (_insideString)
        {
            _insideString = false;
            setChannel(SkipChannel);
            popMode();
        }
    }

    protected bool ShouldPushHereDocMode(pos)
    {
        return _input.LA(pos) == '\r' || _input.LA(pos) == '\n';
    }

    protected bool IsCurlyDollar(pos) {
        return _input.LA(pos) == '$';
    }

    protected void SetInsideString() {
        _insideString = true;
    }
}