/*
 PHP grammar.
 
 The MIT License (MIT).
 Copyright (c) 2015-2017, Ivan Kochurkin (kvanttt@gmail.com), Positive
 Technologies.
 Copyright (c) 2019, Thierry Marianne (thierry.marianne@weaving-the-web.org)

 Permission is hereby granted, free of charge, to any person obtaining a copy of this software and
 associated documentation files (the "Software"), to deal in the Software without restriction,
 including without limitation the rights to use, copy, modify, merge, publish, distribute,
 sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is
 furnished to do so, subject to the following conditions:
 
 The above copyright notice and this permission notice shall be included in all copies or
 substantial portions of the Software.
 
 THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT
 NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
 NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM,
 DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT
 OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
 */

const antlr4 = require("antlr4/index");
const CommonToken = require('antlr4/Token').CommonToken;

function PhpBaseLexer(input) {
    antlr4.Lexer.call(this, input);
    this.AspTags = true;
    this._scriptTag = false;
    this._styleTag = false;
    this._heredocIdentifier = undefined;
    this._prevTokenType = 0;
    this._htmlNameText = undefined;
    this._phpScript = false;
    this._insideString = false;
}

PhpBaseLexer.prototype = Object.create(antlr4.Lexer.prototype);
PhpBaseLexer.prototype.constructor = antlr4.Lexer;

PhpBaseLexer.prototype.nextToken = function() {
    let token = antlr4.Lexer.prototype.nextToken.call(this);

    if (token.type === this.PHPEnd || token.type === this.PHPEndSingleLineComment) {
        if (this._mode === this.SingleLineCommentMode) {
            // SingleLineCommentMode for such allowed syntax:
            // // <?php echo "Hello world"; // comment ?>
            this.popMode();
        }
        this.popMode();

        if (token.text === "</script>") {
            this._phpScript = false;
            token.type = this.ScriptClose;
        } else {
            // Add semicolon to the end of statement if it is absent.
            // For example: <?php echo "Hello world" ?>
            if (this._prevTokenType === this.SemiColon || this._prevTokenType === this.Colon || this._prevTokenType === this.OpenCurlyBracket || this._prevTokenType === this.CloseCurlyBracket) {
                token = antlr4.Lexer.prototype.nextToken.call(this);
            } else {
                token = CommonToken(type=this.SemiColon);
                token.text = ';';
            }
        }
    }

    else if (token.type === this.HtmlName) {
        this._htmlNameText = token.text
    }

    else if (token.type === this.HtmlDoubleQuoteString) {
        if (token.text === "php" && this._htmlNameText === "language") {
            this._phpScript = true;
        }
    }

    else if (this._mode === this.HereDoc) {
        // Heredoc and Nowdoc syntax support: http://php.net/manual/en/language.types.string.php#language.types.string.syntax.heredoc
        if (token.type === this.StartHereDoc || token.type === this.StartNowDoc) {
            this._heredocIdentifier = token.text.slice(3).trim().replace(/\'$/, '');
        }

        if (token.type === this.HereDocText) {
            if (this.CheckHeredocEnd(token.text)) {
                this.popMode()
                const heredocIdentifier = this.GetHeredocEnd(token.text)
                if (token.text.trim().endsWith(';')) {
                    token = new CommonToken(CommonToken.EMPTY_SOURCE, type=this.SemiColon);
                    token.text = `${heredocIdentifier};\n`;
                } else {
                    token = antlr4.Lexer.prototype.nextToken.call(this);
                    token.text = `${heredocIdentifier}\n;`;
                }
            }
        }
    }

    else if (this._mode === this.PHP) {
        if (this._channel === this.HIDDEN) {
            this._prevTokenType = token.type;
        }
    }

    return token;
};

PhpBaseLexer.prototype.GetHeredocEnd = function(text) {
    return text.trim().replace(/\;$/, "");
};

PhpBaseLexer.prototype.CheckHeredocEnd = function(text) {
    return this.GetHeredocEnd(text) === this._heredocIdentifier;
};

PhpBaseLexer.prototype.IsNewLineOrStart = function (pos) {
    return this._input.LA(pos) <= 0 || this._input.LA(pos) == '\r'.charCodeAt(0) ||
        this._input.LA(pos) == '\n'.charCodeAt(0)
};

PhpBaseLexer.prototype.PushModeOnHtmlClose = function () {
    this.popMode();
    if (this._scriptTag) {
        if (!this._phpScript) {
            this.pushMode(this.SCRIPT);
        } else {
            this.pushMode(this.PHP);
        }
        this._scriptTag = false;
    } else if (this._styleTag) {
        this.pushMode(this.STYLE);
        this._styleTag = false;
    }
};
PhpBaseLexer.prototype.HasAspTags = function () {
    return this.AspTags;
};

PhpBaseLexer.prototype.HasPhpScriptTag = function () {
    return this._phpScript;
};

PhpBaseLexer.prototype.PopModeOnCurlyBracketClose = function () {
    if (this._insideString) {
        this._insideString = false;
        this.skip;
        this.popMode();
    }
};

PhpBaseLexer.prototype.ShouldPushHereDocMode = function (pos) {
    return this._input.LA(pos) === '\r'.charCodeAt(0) || this._input.LA(pos) === '\n'.charCodeAt(0);
};

PhpBaseLexer.prototype.IsCurlyDollar = function (pos) {
    return this._input.LA(pos) === '$'.charCodeAt(0);
};

PhpBaseLexer.prototype.SetInsideString = function () {
    this._insideString = true
};

exports.PhpBaseLexer = PhpBaseLexer;
