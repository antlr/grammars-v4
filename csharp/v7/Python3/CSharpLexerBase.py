import sys
from antlr4 import Lexer, Token
from antlr4.Token import CommonToken
from antlr4.InputStream import InputStream


ALL_SEMANTIC_FUNCTIONS = ["IsLocalVariableDeclaration"]


class CSharpLexerBase(Lexer):

    def __init__(self, input: InputStream, output=sys.stdout):
        super().__init__(input, output)
        self.interpolatedStringLevel = 0
        self.interpolatedVerbatiums = []
        self.curlyLevels = []
        self.verbatium = False

        # Preprocessor state
        self._pending = []
        self._symbols = set()
        self._condition = []
        self._taken = []

        # Expression evaluator cursor
        self._expr = []
        self._epos = 0

        self._initPreprocessor()

    def OnInterpolatedRegularStringStart(self):
        self.interpolatedStringLevel += 1
        self.interpolatedVerbatiums.append(False)
        self.verbatium = False

    def OnInterpolatedVerbatiumStringStart(self):
        self.interpolatedStringLevel += 1
        self.interpolatedVerbatiums.append(True)
        self.verbatium = True

    def OnOpenBrace(self):
        if self.interpolatedStringLevel > 0:
            self.curlyLevels[-1] += 1

    def OnCloseBrace(self):
        if self.interpolatedStringLevel > 0:
            self.curlyLevels[-1] -= 1
            if self.curlyLevels[-1] == 0:
                self.curlyLevels.pop()
                self.skip()
                self.popMode()

    def OnColon(self):
        if self.interpolatedStringLevel > 0:
            ind = 1
            switchToFormatString = True
            while chr(self.inputStream.LA(ind)) != '}':
                ch = self.inputStream.LA(ind)
                if ch == ord(':') or ch == ord(')'):
                    switchToFormatString = False
                    break
                ind += 1
            if switchToFormatString:
                if "." in __name__:
                    from .CSharpLexer import CSharpLexer
                else:
                    from CSharpLexer import CSharpLexer
                self.mode(CSharpLexer.INTERPOLATION_FORMAT)

    def OpenBraceInside(self):
        self.curlyLevels.append(1)

    def OnDoubleQuoteInside(self):
        self.interpolatedStringLevel -= 1
        self.interpolatedVerbatiums.pop()
        self.verbatium = (len(self.interpolatedVerbatiums) > 0
                          and self.interpolatedVerbatiums[-1])

    def OnCloseBraceInside(self):
        self.curlyLevels.pop()

    def IsRegularCharInside(self):
        return not self.verbatium

    def IsVerbatiumDoubleQuoteInside(self):
        return self.verbatium

    # -------------------------------------------------------------------------
    # Preprocessor initialisation
    # -------------------------------------------------------------------------
    def _initPreprocessor(self):
        for arg in sys.argv:
            if arg.startswith("--D"):
                for sym in arg[3:].split(';'):
                    if sym:
                        self._symbols.add(sym)

    def _isActive(self):
        return len(self._condition) == 0 or self._condition[-1]

    # -------------------------------------------------------------------------
    # nextToken override — intercepts DIRECTIVE-channel tokens
    # -------------------------------------------------------------------------
    def nextToken(self):
        if "." in __name__:
            from .CSharpLexer import CSharpLexer
        else:
            from CSharpLexer import CSharpLexer

        if self._pending:
            return self._pending.pop(0)

        tok = super().nextToken()

        if tok.channel == CSharpLexer.DIRECTIVE:
            skipped = None
            t = tok.type
            if   t == CSharpLexer.DEFINE: self._handleDefine(CSharpLexer)
            elif t == CSharpLexer.UNDEF:  self._handleUndef(CSharpLexer)
            elif t == CSharpLexer.IF:     skipped = self._handleIf(CSharpLexer)
            elif t == CSharpLexer.ELIF:   skipped = self._handleElif(CSharpLexer)
            elif t == CSharpLexer.ELSE:   skipped = self._handleElse(CSharpLexer)
            elif t == CSharpLexer.ENDIF:  self._handleEndif(CSharpLexer)
            if skipped is not None:
                self._pending.append(skipped)

        return tok

    # -------------------------------------------------------------------------
    # Directive handlers
    # -------------------------------------------------------------------------
    def _handleDefine(self, CSharpLexer):
        line = self._collectLine(CSharpLexer)
        sym = self._symbolFromLine(line, CSharpLexer)
        if self._isActive() and sym is not None:
            self._symbols.add(sym)

    def _handleUndef(self, CSharpLexer):
        line = self._collectLine(CSharpLexer)
        sym = self._symbolFromLine(line, CSharpLexer)
        if self._isActive() and sym is not None:
            self._symbols.discard(sym)

    def _handleIf(self, CSharpLexer):
        line = self._collectLine(CSharpLexer)
        outer = self._isActive()
        result = outer and self._evaluate(line, CSharpLexer)
        self._condition.append(result)
        self._taken.append(result)
        return None if result else self._skipFalseBlock(CSharpLexer)

    def _handleElif(self, CSharpLexer):
        line = self._collectLine(CSharpLexer)
        alreadyTaken = self._taken.pop() if self._taken else False
        if self._condition:
            self._condition.pop()
        outer = self._isActive()
        result = not alreadyTaken and outer and self._evaluate(line, CSharpLexer)
        self._condition.append(result)
        self._taken.append(alreadyTaken or result)
        return None if result else self._skipFalseBlock(CSharpLexer)

    def _handleElse(self, CSharpLexer):
        self._collectLine(CSharpLexer)
        alreadyTaken = self._taken.pop() if self._taken else False
        if self._condition:
            self._condition.pop()
        outer = self._isActive()
        result = not alreadyTaken and outer
        self._condition.append(result)
        self._taken.append(True)
        return None if result else self._skipFalseBlock(CSharpLexer)

    def _handleEndif(self, CSharpLexer):
        self._collectLine(CSharpLexer)
        if self._condition:
            self._condition.pop()
        if self._taken:
            self._taken.pop()

    # -------------------------------------------------------------------------
    # _collectLine — drain DIRECTIVE_MODE tokens up to DIRECTIVE_NEW_LINE
    # -------------------------------------------------------------------------
    def _collectLine(self, CSharpLexer):
        tokens = []
        while True:
            t = super().nextToken()
            if t.channel != Lexer.HIDDEN and t.channel != CSharpLexer.COMMENTS_CHANNEL:
                tokens.append(t)
            if t.type == CSharpLexer.DIRECTIVE_NEW_LINE or t.type == Token.EOF:
                break
        return tokens

    def _symbolFromLine(self, line, CSharpLexer):
        for t in line:
            if t.type == CSharpLexer.CONDITIONAL_SYMBOL:
                return t.text
        return None

    # -------------------------------------------------------------------------
    # _skipFalseBlock — scan char stream, return SKIPPED_SECTION on HIDDEN channel
    # -------------------------------------------------------------------------
    def _skipFalseBlock(self, CSharpLexer):
        text = []
        stream = self.inputStream
        depth = 1
        atLineStart = True
        startLine = self.line

        while True:
            c = stream.LA(1)
            if c == Token.EOF:
                break

            if c in (0x0D, 0x0A, 0x85, 0x2028, 0x2029):
                stream.consume()
                text.append(chr(c))
                if c == 0x0D and stream.LA(1) == 0x0A:
                    stream.consume()
                    text.append('\n')
                atLineStart = True
                continue

            if atLineStart and c in (0x20, 0x09):
                stream.consume()
                text.append(chr(c))
                continue

            if atLineStart and c == 0x23:  # '#'
                kw = self._peekKeyword()
                if kw == "if":
                    depth += 1
                elif kw == "endif":
                    depth -= 1
                    if depth == 0:
                        break
                elif (kw == "else" or kw == "elif") and depth == 1:
                    break

            atLineStart = False
            stream.consume()
            text.append(chr(c))

        tok = CommonToken(type=CSharpLexer.SKIPPED_SECTION, text="".join(text))
        tok.channel = Lexer.HIDDEN
        tok.line = startLine
        return tok

    def _peekKeyword(self):
        i = 2  # LA(1) is '#'
        while self.inputStream.LA(i) in (0x20, 0x09):
            i += 1
        kw = []
        while True:
            c = self.inputStream.LA(i)
            if c == -1:
                break
            ch = chr(c)
            if not ch.isalpha():
                break
            kw.append(ch)
            i += 1
        return "".join(kw)

    # -------------------------------------------------------------------------
    # Recursive-descent expression evaluator
    # -------------------------------------------------------------------------
    def _evaluate(self, tokens, CSharpLexer):
        self._expr = tokens
        self._epos = 0
        return self._parseOr(CSharpLexer)

    def _peekType(self, CSharpLexer):
        if self._epos < len(self._expr):
            t = self._expr[self._epos].type
            if t != CSharpLexer.DIRECTIVE_NEW_LINE and t != Token.EOF:
                return t
        return -1

    def _eConsume(self):
        tok = self._expr[self._epos]
        self._epos += 1
        return tok

    def _parseOr(self, CSharpLexer):
        v = self._parseAnd(CSharpLexer)
        while self._peekType(CSharpLexer) == CSharpLexer.OP_OR:
            self._eConsume()
            v = self._parseAnd(CSharpLexer) or v
        return v

    def _parseAnd(self, CSharpLexer):
        v = self._parseEq(CSharpLexer)
        while self._peekType(CSharpLexer) == CSharpLexer.OP_AND:
            self._eConsume()
            v = self._parseEq(CSharpLexer) and v
        return v

    def _parseEq(self, CSharpLexer):
        v = self._parseUnary(CSharpLexer)
        if self._peekType(CSharpLexer) == CSharpLexer.OP_EQ:
            self._eConsume()
            return v == self._parseUnary(CSharpLexer)
        if self._peekType(CSharpLexer) == CSharpLexer.OP_NE:
            self._eConsume()
            return v != self._parseUnary(CSharpLexer)
        return v

    def _parseUnary(self, CSharpLexer):
        if self._peekType(CSharpLexer) == CSharpLexer.BANG:
            self._eConsume()
            return not self._parseUnary(CSharpLexer)
        return self._parsePrimary(CSharpLexer)

    def _parsePrimary(self, CSharpLexer):
        t = self._peekType(CSharpLexer)
        if t == CSharpLexer.TRUE:
            self._eConsume()
            return True
        if t == CSharpLexer.FALSE:
            self._eConsume()
            return False
        if t == CSharpLexer.CONDITIONAL_SYMBOL:
            return self._eConsume().text in self._symbols
        if t == CSharpLexer.OPEN_PARENS:
            self._eConsume()
            v = self._parseOr(CSharpLexer)
            if self._peekType(CSharpLexer) == CSharpLexer.CLOSE_PARENS:
                self._eConsume()
            return v
        return False
