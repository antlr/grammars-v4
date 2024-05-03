
// $antlr-format alignColons trailing, alignLabels true, alignLexerCommands true, alignSemicolons ownLine, alignTrailers true
// $antlr-format alignTrailingComments true, allowShortBlocksOnASingleLine true, allowShortRulesOnASingleLine true, columnLimit 150
// $antlr-format maxEmptyLinesToKeep 1, minEmptyLines 0, reflowComments false, singleLineOverrulesHangingColon true, useTab false

lexer grammar PestLexer;

channels {
    OFF_CHANNEL
}

UNDERSCORE          : '_';
AT                  : '@';
DOLLAR              : '$';
NOT                 : '!';
AMP                 : '&';
TILDE               : '~';
VBAR                : '|';
QUESTION            : '?';
STAR                : '*';
PLUS                : '+';
ASSIGNMENT_OPERATOR : '=';
OPENING_BRACE       : '{';
CLOSING_BRACE       : '}';
OPENING_PAREN       : '(';
CLOSING_PAREN       : ')';
OPENING_BRACK       : '[';
CLOSING_BRACK       : ']';
PUSH                : 'PUSH';
PEEK                : 'PEEK';
NUMBER              : '0' ..'9'+;
INTEGER             : NUMBER | '-' '0'* '1' ..'9' NUMBER?;
COMMA               : ',';
IDENTIFIER          : ('_' | ALPHA) ('_' | ALPHA_NUM)*;
fragment ALPHA      : 'a' ..'z' | 'A' ..'Z';
fragment ALPHA_NUM  : ALPHA | '0' ..'9';
STRING              : QUOTE ( ESCAPE | ~["\r\n\\])* QUOTE;
INSENSITIVE_STRING  : '^' STRING;
RANGE               : CHARACTER RANGE_OPERATOR CHARACTER;
CHARACTER           : SINGLE_QUOTE INNER_CHR SINGLE_QUOTE;
fragment INNER_STR  : ANY | ESCAPE;
fragment INNER_CHR  : ESCAPE | ANY;
ESCAPE              : '\\' ('"' | '\\' | 'r' | 'n' | 't' | '0' | '\'' | CODE | UNICODE);
CODE                : 'X' HEX_DIGIT HEX_DIGIT;
UNICODE:
    [uU] OPENING_BRACE (
        HEX_DIGIT HEX_DIGIT
        | HEX_DIGIT HEX_DIGIT HEX_DIGIT HEX_DIGIT
        | HEX_DIGIT HEX_DIGIT HEX_DIGIT HEX_DIGIT HEX_DIGIT HEX_DIGIT
    ) CLOSING_BRACE
;
HEX_DIGIT                : '0' ..'9' | 'a' ..'f' | 'A' ..'F';
QUOTE                    : '"';
SINGLE_QUOTE             : '\'';
RANGE_OPERATOR           : '..';
WHITESPACE               : (' ' | '\t' | NEWLINE)+           -> channel(OFF_CHANNEL);
BLOCK_COMMENT            : BLOCK_COMMENT_F                   -> channel(OFF_CHANNEL);
COMMENT                  : (BLOCK_COMMENT_F | '//' ~[\r\n]*) -> channel(OFF_CHANNEL);
fragment BLOCK_COMMENT_F : '/*' (COMMENT | ANY)*? '*/';
fragment NEWLINE         : [\r\n]+;
fragment ANY             : .;