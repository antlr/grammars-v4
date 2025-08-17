from antlr4 import *
import RexxLexer

class RexxParserBase(Parser):
   def is_prev_token_whitespace(self):
      tokenIndex = self.getCurrentToken().tokenIndex
      prevTokenType = self.getTokenStream().get(tokenIndex-1).type
      return (prevTokenType is RexxLexer.RexxLexer.WHITESPACES)

   def is_prev_token_not_whitespace(self):
      return not self.is_prev_token_whitespace()
