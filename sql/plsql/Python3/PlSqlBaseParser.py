from antlr4 import *

class PlSqlBaseParser(Parser):

    _isVersion10 = False
    _isVersion12 = True

    def isVersion10(self):
        return self._isVersion10

    def isVersion12(self):
        return self._isVersion12

    def setVersion10(self, value):
        self._isVersion10 = value

    def setVersion12(self, value):
        self._isVersion12 = value