
from antlr4 import *

relativeImport = False
if __name__ is not None and "." in __name__:
    relativeImport = True

class JavaScriptLexerBase(Lexer):
    def __init__(self, *args, **kwargs):
        super(JavaScriptLexerBase, self).__init__(*args, **kwargs)

        """Stores values of nested modes. By default mode is strict or
        defined externally (useStrictDefault)"""
        self.scopeStrictModes = []
        self.lastToken: Token = None

        """Default value of strict mode
        Can be defined externally by setUseStrictDefault"""
        self.useStrictDefault = False

        """Current value of strict mode
        Can be defined during parsing, see StringFunctions.js and StringGlobal.js samples"""
        self.useStrictCurrent = False

    def getStrictDefault(self) -> bool:
        return self.useStrictDefault

    def setUseStrictDefault(self, value: bool):
        self.useStrictDefault = value
        self.useStrictCurrent = value

    def isStrictMode(self):
        return self.useStrictCurrent

    def isStartOfFile(self):
        return self.lastToken is None

    def nextToken(self) -> Token:
        """Return the next token from the character stream and records this last
        token in case it resides on the default channel. This recorded token
        is used to determine when the lexer could possibly match a regex
        literal. Also changes scopeStrictModes stack if tokenize special
        string 'use strict';

        :return the next token from the character stream."""
        next_token: Token = super(JavaScriptLexerBase, self).nextToken()

        if next_token.channel == Token.DEFAULT_CHANNEL:
            self.lastToken = next_token

        return next_token

    def processOpenBrace(self):
        self.useStrictCurrent = bool(self.scopeStrictModes) and (True if self.scopeStrictModes[-1] else self.useStrictDefault)
        self.scopeStrictModes.append(self.useStrictCurrent)

    def processCloseBrace(self):
        self.useStrictCurrent = bool(self.scopeStrictModes) and (True if self.scopeStrictModes.pop(-1) else self.useStrictDefault)

    def processStringLiteral(self):
        if relativeImport:
            from .JavaScriptLexer import JavaScriptLexer
        else:
            from JavaScriptLexer import JavaScriptLexer
        if not self.lastToken or self.lastToken.type == JavaScriptLexer.OpenBrace:
            text = self.text
            if text == '"use strict"' or text == "'use strict'":
                if self.scopeStrictModes:
                    self.scopeStrictModes.pop(-1)
                self.useStrictCurrent = True
                self.scopeStrictModes.append(self.useStrictCurrent)

    def isRegexPossible(self) -> bool:
        """Returns {@code true} if the lexer can match a regex literal. """
        if relativeImport:
            from .JavaScriptLexer import JavaScriptLexer
        else:
            from JavaScriptLexer import JavaScriptLexer

        if not self.lastToken:
            # No token has been produced yet: at the start of the input,
            # no division is possible, so a regex literal _is_ possible.
            return True

        if self.lastToken.type in [
                JavaScriptLexer.Identifier,
                JavaScriptLexer.NullLiteral,
                JavaScriptLexer.BooleanLiteral,
                JavaScriptLexer.This,
                JavaScriptLexer.CloseBracket,
                JavaScriptLexer.CloseParen,
                JavaScriptLexer.OctalIntegerLiteral,
                JavaScriptLexer.DecimalLiteral,
                JavaScriptLexer.HexIntegerLiteral,
                JavaScriptLexer.StringLiteral,
                JavaScriptLexer.PlusPlus,
                JavaScriptLexer.MinusMinus]:
            return False

        return True
