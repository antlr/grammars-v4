lexer grammar LuaLexer;

options { superClass = LuaLexerBase; }

// Insert here @header for C++ lexer.

SEMI: ';';
EQ: '=';

BREAK: 'break';
GOTO: 'goto';
DO: 'do';
END: 'end';
WHILE: 'while';
REPEAT: 'repeat';
UNTIL: 'until';
IF: 'if';
THEN: 'then';
ELSEIF: 'elseif';
ELSE: 'else';
FOR: 'for';
COMMA: ',';
IN: 'in';
FUNCTION: 'function';
LOCAL: 'local';
LT: '<';
GT: '>';
RETURN: 'return';
CONTINUE: 'continue';
CC: '::';
NIL: 'nil';
FALSE: 'false';
TRUE: 'true';
DOT: '.';
SQUIG: '~';
MINUS: '-';
POUND: '#';
OP: '(';
CP: ')';
NOT: 'not';
LL: '<<';
GG: '>>';
AMP: '&';
SS: '//';
PER: '%';
COL: ':';
LE: '<=';
GE: '>=';
AND: 'and';
OR: 'or';
PLUS: '+';
STAR: '*';
OCU: '{';
CCU: '}';
OB: '[';
CB: ']';
EE: '==';
DD: '..';
PIPE: '|';
CARET: '^';
SLASH: '/';
DDD: '...';
SQEQ: '~=';


NAME
    : [a-zA-Z_][a-zA-Z_0-9]*
    ;

NORMALSTRING
    : '"' ( EscapeSequence | ~('\\'|'"') )* '"'
    ;

CHARSTRING
    : '\'' ( EscapeSequence | ~('\''|'\\') )* '\''
    ;

LONGSTRING
    : '[' NESTED_STR ']'
    ;

fragment
NESTED_STR
    : '=' NESTED_STR '='
    | '[' .*? ']'
    ;

INT
    : Digit+
    ;

HEX
    : '0' [xX] HexDigit+
    ;

FLOAT
    : Digit+ '.' Digit* ExponentPart?
    | '.' Digit+ ExponentPart?
    | Digit+ ExponentPart
    ;

HEX_FLOAT
    : '0' [xX] HexDigit+ '.' HexDigit* HexExponentPart?
    | '0' [xX] '.' HexDigit+ HexExponentPart?
    | '0' [xX] HexDigit+ HexExponentPart
    ;

fragment
ExponentPart
    : [eE] [+-]? Digit+
    ;

fragment
HexExponentPart
    : [pP] [+-]? Digit+
    ;

fragment
EscapeSequence
    : '\\' [abfnrtvz"'|$#\\]   // World of Warcraft Lua additionally escapes |$# 
    | '\\' '\r'? '\n'
    | DecimalEscape
    | HexEscape
    | UtfEscape
    ;

fragment
DecimalEscape
    : '\\' Digit
    | '\\' Digit Digit
    | '\\' [0-2] Digit Digit
    ;

fragment
HexEscape
    : '\\' 'x' HexDigit HexDigit
    ;

fragment
UtfEscape
    : '\\' 'u{' HexDigit+ '}'
    ;

fragment
Digit
    : [0-9]
    ;

fragment
HexDigit
    : [0-9a-fA-F]
    ;

fragment
SingleLineInputCharacter
    : ~[\r\n\u0085\u2028\u2029]
    ;

COMMENT
    : '--' { this.HandleComment(); } -> channel(HIDDEN)
    ;

WS
    : [ \t\u000C\r]+ -> channel(HIDDEN)
    ;

NL: [\n] -> channel(2);

SHEBANG
    : '#' { this.IsLine1Col0() }? '!'? SingleLineInputCharacter* -> channel(HIDDEN)
    ;

