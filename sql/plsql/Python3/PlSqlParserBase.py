from antlr4 import *

class PlSqlParserBase(Parser):

    _isVersion10 = False
    _isVersion11 = False
    _isVersion12 = True

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