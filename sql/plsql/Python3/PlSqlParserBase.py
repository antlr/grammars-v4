from antlr4 import *

class PlSqlParserBase(Parser):

    _isVersion10 = False
    _isVersion11 = True
    _isVersion12 = True

    def __init__(self, input, output=None):
        super().__init__(input, output)
        self._lastUnitWasPlsql = False

    def reset(self):
        self._lastUnitWasPlsql = False
        super().reset()

    def setLastUnitPlsql(self):
        self._lastUnitWasPlsql = True

    def setLastUnitSql(self):
        self._lastUnitWasPlsql = False

    def isLastUnitSql(self):
        return not self._lastUnitWasPlsql

    def isLastUnitPlsql(self):
        return self._lastUnitWasPlsql

    def isSolidusSeparator(self):
        from PlSqlLexer import PlSqlLexer as _Lexer
        solidus = self._input.LT(1)
        if solidus is None or solidus.type != _Lexer.SOLIDUS:
            return False

        solidusLine = solidus.line

        prev = self._input.LT(-1)
        if prev is not None and prev.type != Token.EOF and prev.line == solidusLine:
            return False

        next = self._input.LT(2)
        if next is not None and next.type != Token.EOF and next.line == solidusLine:
            return False

        return True

    def isVersion10(self):
        return self._isVersion10

    def isVersion11(self):
        return self._isVersion11

    def isVersion12(self):
        return self._isVersion12

    def setVersion10(self, value):
        self._isVersion10 = value

    def setVersion11(self, value):
        self._isVersion11 = value

    def setVersion12(self, value):
        self._isVersion12 = value

    def IsNotNumericFunction(self):
        from PlSqlLexer import PlSqlLexer as _Lexer
        lt1 = self._input.LT(1)
        lt2 = self._input.LT(2)
        if (lt1.type in (_Lexer.SUM, _Lexer.COUNT, _Lexer.AVG,
                         _Lexer.MIN, _Lexer.MAX, _Lexer.ROUND,
                         _Lexer.LEAST, _Lexer.GREATEST) and
                lt2.type == _Lexer.LEFT_PAREN):
            return False
        return True

    def isNotStartOfJoin(self):
        from PlSqlLexer import PlSqlLexer as _Lexer
        lt1 = self._input.LT(1)
        if (lt1.type in (_Lexer.INNER, _Lexer.CROSS, _Lexer.NATURAL,
                         _Lexer.PARTITION, _Lexer.FULL, _Lexer.LEFT,
                         _Lexer.RIGHT, _Lexer.OUTER)):
            return False
        return True
