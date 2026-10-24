from antlr4 import Lexer, Token


class awkLexerBase(Lexer):
    def __init__(self, *args):
        super().__init__(*args)
        self._afterExpr = False

    def nextToken(self):
        token = super().nextToken()
        if token.channel == Token.DEFAULT_CHANNEL:
            from awkLexer import awkLexer
            self._afterExpr = (
                token.type == awkLexer.WORD
                or token.type == awkLexer.NUMBER
                or token.type == awkLexer.STRING
                or token.type == awkLexer.BUILTIN_FUNC_NAME
                or token.type == awkLexer.INCR
                or token.type == awkLexer.DECR
                or token.type == awkLexer.Rp
                or token.type == awkLexer.Rb
            )
        return token

    def IsNotAfterExpr(self):
        return not self._afterExpr
