"""
PHP grammar.
The MIT License (MIT).
Copyright (c) 2015-2017, Ivan Kochurkin (kvanttt@gmail.com), Positive Technologies.
Copyright (c) 2016, Jorrit Kronjee (Python port)
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
"""

from antlr4 import *
from antlr4.Token import CommonToken


class PhpLexerBase(Lexer):
    AspTags = True
    _scriptTag = False
    _styleTag = False
    _heredocIdentifier = None
    _prevTokenType = 0
    _htmlNameText = None
    _phpScript = False
    _insideString = False

    def nextToken(self):
        token = super(PhpLexerBase, self).nextToken()

        if token.type == self.PHPEnd or token.type == self.PHPEndSingleLineComment:
            if self._mode == self.SingleLineCommentMode:
                # SingleLineCommentMode for such allowed syntax:
                # // <?php echo "Hello world"; // comment ?>
                self.popMode()
            self.popMode()

            if token.text == "</script>":
                self._phpScript = False
                token.type = self.HtmlScriptClose
            else:
                # Add semicolon to the end of statement if it is absent.
                # For example: <?php echo "Hello world" ?>
                if self._prevTokenType == self.SemiColon or \
                        self._prevTokenType == self.Colon or \
                        self._prevTokenType == self.OpenCurlyBracket or \
                        self._prevTokenType == self.CloseCurlyBracket:
                    token = super(PhpLexerBase, self).nextToken()
                else:
                    token = CommonToken(type=self.SemiColon)
                    token.text = ';'
        elif token.type == self.HtmlName:
            self._htmlNameText = token.text
        elif token.type == self.HtmlDoubleQuoteString:
            if token.text == "php" and self._htmlNameText == "language":
                self._phpScript = True
        elif self._mode == self.HereDoc:
            # Heredoc and Nowdoc syntax support: http://php.net/manual/en/language.types.string.php#language.types.string.syntax.heredoc
            if token.type == self.StartHereDoc or token.type == self.StartNowDoc:
                self._heredocIdentifier = token.text[3:].strip().replace("'", "")
            if token.type == self.HereDocText:
                if self.CheckHeredocEnd(token.text):
                    self.popMode()
                    heredoc_identifier = self.GetHeredocEnd(token.text)
                    if token.text.strip().endswith(';'):
                        text = heredoc_identifier + ";\n"
                        token = CommonToken(type=self.SemiColon)
                        token.text = text
                    else:
                        token = super(PhpLexerBase, self).nextToken()
                        token.text = heredoc_identifier + "\n;"
        elif self._mode == self.PHP:
            if self._channel == self.HIDDEN:
                self._prevTokenType = token.type

        return token

    def GetHeredocEnd(self, text):
        return text.strip().rstrip(';')

    def CheckHeredocEnd(self, text):
        return self.GetHeredocEnd(text) == self._heredocIdentifier

    def IsNewLineOrStart(self, pos):
        return self._input.LA(-1) <= 0 or self._input.LA(-1) == ord('\r') or self._input.LA(-1) == ord('\n')

    def PushModeOnHtmlClose(self):
        self.popMode()
        if self._scriptTag:
            if not self._phpScript:
                self.pushMode(self.SCRIPT)
            else:
                self.pushMode(self.PHP)
            self._scriptTag = False
        elif self._styleTag:
            self.pushMode(self.STYLE)
            self._styleTag = False

    def HasAspTags(self):
        return self.AspTags

    def HasPhpScriptTag(self):
        return self._phpScript

    def PopModeOnCurlyBracketClose(self):
        if self._insideString:
            self._insideString = False
            self._channel = self.SKIP
            self.popMode()

    def ShouldPushHereDocMode(self, pos):
        return self._input.LA(pos) == ord('\r') or self._input.LA(pos) == ord('\n')

    def IsCurlyDollar(self, pos):
        return self._input.LA(pos) == ord('$')

    def SetInsideString(self):
        self._insideString = True
