import sys
from antlr4 import Lexer, Token
from antlr4.Token import CommonToken
from antlr4.InputStream import InputStream


class CSharpLexerBase(Lexer):

    def __init__(self, input: InputStream, output=sys.stdout):
        super().__init__(input, output)

        # Preprocessor state
        self._pending = []
        self._symbols = set()
        self._condition = []
        self._taken = []

        # Expression evaluator cursor
        self._expr = []
        self._epos = 0

        self._initPreprocessor()

    # -------------------------------------------------------------------------
    # Mode-stack helpers
    # -------------------------------------------------------------------------
    def PeekMode(self):
        stack = self._modeStack
        return stack[-1] if stack else self.DEFAULT_MODE

    def PopMode(self):
        if not self._modeStack:
            print("unbalanced ()/{}/[]", file=sys.stderr)
            return self.DEFAULT_MODE
        return super().popMode()

    def PeekModeIs(self, mode):
        return self.PeekMode() == mode

    def LookAheadIs(self, pos, value):
        return self.inputStream.LA(pos) == value

    def LookAheadIsNot(self, pos, value):
        return self.inputStream.LA(pos) != value

    def LookAheadIsRBrace1(self):
        return self.inputStream.LA(1) == 125

    def LookAheadIsNotLBrace2(self):
        return self.inputStream.LA(2) != 123

    def PeekModeIsIrsCont(self):
        if "." in __name__:
            from .CSharpLexer import CSharpLexer
        else:
            from CSharpLexer import CSharpLexer
        return self.PeekModeIs(CSharpLexer.IRS_CONT)

    def PeekModeIsIvsCont(self):
        if "." in __name__:
            from .CSharpLexer import CSharpLexer
        else:
            from CSharpLexer import CSharpLexer
        return self.PeekModeIs(CSharpLexer.IVS_CONT)

    def WrapToken(self):
        self.text = "\u3014" + self.text.replace("\u3015", "\u3015\u3015") + "\u3015"

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
            if   t == CSharpLexer.DEFINE:  self._handleDefine(CSharpLexer)
            elif t == CSharpLexer.UNDEF:   self._handleUndef(CSharpLexer)
            elif t == CSharpLexer.KW_IF:   skipped = self._handleIf(CSharpLexer)
            elif t == CSharpLexer.ELIF:    skipped = self._handleElif(CSharpLexer)
            elif t == CSharpLexer.KW_ELSE: skipped = self._handleElse(CSharpLexer)
            elif t == CSharpLexer.ENDIF:   self._handleEndif(CSharpLexer)
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
            if t.channel != Lexer.HIDDEN:
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
        while self._peekType(CSharpLexer) == CSharpLexer.TK_OR_OR:
            self._eConsume()
            v = self._parseAnd(CSharpLexer) or v
        return v

    def _parseAnd(self, CSharpLexer):
        v = self._parseEq(CSharpLexer)
        while self._peekType(CSharpLexer) == CSharpLexer.TK_AND_AND:
            self._eConsume()
            v = self._parseEq(CSharpLexer) and v
        return v

    def _parseEq(self, CSharpLexer):
        v = self._parseUnary(CSharpLexer)
        if self._peekType(CSharpLexer) == CSharpLexer.TK_EQ_EQ:
            self._eConsume()
            return v == self._parseUnary(CSharpLexer)
        if self._peekType(CSharpLexer) == CSharpLexer.TK_NOT_EQ:
            self._eConsume()
            return v != self._parseUnary(CSharpLexer)
        return v

    def _parseUnary(self, CSharpLexer):
        if self._peekType(CSharpLexer) == CSharpLexer.TK_NOT:
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
        if t == CSharpLexer.TK_LPAREN:
            self._eConsume()
            v = self._parseOr(CSharpLexer)
            if self._peekType(CSharpLexer) == CSharpLexer.TK_RPAREN:
                self._eConsume()
            return v
        return False
