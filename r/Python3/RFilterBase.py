import sys
from antlr4 import *
from antlr4.Token import CommonToken

if sys.version_info[1] > 5:
    from typing import TextIO
else:
    from typing.io import TextIO

class RFilterBase(Parser):
    """
    Base class for RFilter parser with state management for curly braces
    and helper methods for token manipulation.
    """

    def __init__(self, input: TokenStream, output: TextIO = sys.stdout):
        super().__init__(input, output)
        self.curlies = 0

    def hideToken(self, token):
        """
        Set a token to the hidden channel.
        This is the Python equivalent of Java's token.setChannel(Token.HIDDEN_CHANNEL)
        """
        if isinstance(token, CommonToken):
            token.channel = Token.HIDDEN_CHANNEL