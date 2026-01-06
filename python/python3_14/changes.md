# Dec. 24, 2025
- parser grammar update for Python 3.14.2
- tokenizing t-string literals
- tokenizing BOM Unicode character at the start of the file so it is skipped in the token stream
- moved encoding detection from PythonLexerBase to a separate component

# Jan. 07, 2025
- parser grammar update for Python 3.13.1<br/><br/>
- added ENCODING token<br/><br/>
- complete rewrite of fstring tokenizer in lexer grammar and PythonLexerBase class
    - now correctly tokenizes the followings in fstring:
        - escape sequences
        - walrus operator
        - dictionary comprehension
        - set comprehension<br/><br/>
- soft keywords changes:
    - no embedded code (semantic predicates) in parser grammar for soft keywords
    - no need for PythonParserBase class
    - no need for transformGrammar.py
    - BREAKING CHANGES:
        - dedicated tokens for soft keywords instead of NAME token:
          - NAME_OR_TYPE
          - NAME_OR_MATCH
          - NAME_OR_CASE
          - NAME_OR_WILDCARD

# Oct. 18, 2024
- Fix that `case [a, *_] if a == 0:` throws error rule soft_kw__not__wildcard failed predicate:
  `{this.isnotEqualToCurrentTokenText("_")}?`

# Sept. 05, 2024
- Type comment tokens are no longer generated.  
  Type comments will now be tokenized as plain comment tokens.  
  <br/><br/>
- Line continuation for string literals (backslash followed by a newline) is no longer resolved.  
  (backslash+newline is no longer removed from string literals)
