// KumirLexer.g4
// ANTLR v4 Lexer Grammar for the Kumir language.
// Source: Refined based on official documentation and extensive testing
//         against K.Y. Polyakov's examples. Developed collaboratively.
// Author: [Your Name/GitHub Handle]
// License: MIT License

lexer grammar KumirLexer;

options { caseInsensitive = true; }

// --- Keywords (Core Language) ---
// Keywords are case-insensitive (both lowercase and uppercase Cyrillic are matched).
MODULE              : 'модуль';
ENDMODULE           : ('конец' WS 'модуля' | 'конецмодуля' | 'конец_модуля');
ALG_HEADER          : 'алг';
ALG_BEGIN           : 'нач';
ALG_END             : 'кон';
PRE_CONDITION       : 'дано';
POST_CONDITION      : 'надо';
ASSERTION           : 'утв';
LOOP                : 'нц';
ENDLOOP_COND        : ('кц' WS 'при' | 'кц_при');
ENDLOOP             : 'кц';
IF                  : 'если';
THEN                : 'то';
ELSE                : 'иначе';
FI                  : 'все';
SWITCH              : 'выбор';
CASE                : 'при';
INPUT               : 'ввод';
OUTPUT              : 'вывод';
ASSIGN              : ':=';
EXIT                : 'выход';
PAUSE               : 'пауза';
STOP                : 'стоп';
IMPORT              : 'использовать';
FOR                 : 'для';
WHILE               : 'пока';
TIMES               : 'раз';
FROM                : 'от';
TO                  : 'до';
STEP                : 'шаг';
NEWLINE_CONST       : 'нс';
NOT                 : 'не';
AND                 : 'и';
OR                  : 'или';
OUT_PARAM           : 'рез';
IN_PARAM            : 'арг';
INOUT_PARAM         : ('аргрез' | 'арг' WS 'рез' | 'арг_рез');
RETURN_VALUE        : 'знач';

// --- Data Types ---
INTEGER_TYPE        : 'цел';
REAL_TYPE           : 'вещ';
BOOLEAN_TYPE        : 'лог';
CHAR_TYPE           : 'сим';
STRING_TYPE         : 'лит';
TABLE_SUFFIX        : 'таб';
// Actor-specific types
KOMPL_TYPE          : 'компл';
COLOR_TYPE          : 'цвет';
SCANCODE_TYPE       : 'сканкод';
FILE_TYPE           : 'файл';
// Explicit array/table types (handle variations with space, no space, underscore)
INTEGER_ARRAY_TYPE  : ('цел' WS? 'таб' | 'цел_таб');
REAL_ARRAY_TYPE     : ('вещ' WS? 'таб' | 'вещ_таб');
CHAR_ARRAY_TYPE     : ('сим' WS? 'таб' | 'сим_таб');
STRING_ARRAY_TYPE   : ('лит' WS? 'таб' | 'лит_таб');
BOOLEAN_ARRAY_TYPE  : ('лог' WS? 'таб' | 'лог_таб');

// --- Constants ---
TRUE                : 'да';
FALSE               : 'нет';
// Color constants
PROZRACHNIY         : 'прозрачный';
BELIY               : 'белый';
CHERNIY             : 'чёрный' | 'черный';
SERIY               : 'серый';
FIOLETOVIY          : 'фиолетовый';
SINIY               : 'синий';
GOLUBOY             : 'голубой';
ZELENIY             : 'зелёный' | 'зеленый';
ZHELTIY             : 'жёлтый' | 'желтый';
ORANZHEVIY          : 'оранжевый';
KRASNIY             : 'красный';

// --- Operators ---
POWER               : '**';
GE                  : '>=' | '≥';
LE                  : '<=' | '≤';
NE                  : '<>' | '≠';
PLUS                : '+';
MINUS               : '-';
MUL                 : '*';
DIV                 : '/';
EQ                  : '=';
LT                  : '<';
GT                  : '>';
LPAREN              : '(';
RPAREN              : ')';
LBRACK              : '[';
RBRACK              : ']';
LBRACE              : '{';
RBRACE              : '}';
COMMA               : ',';
COLON               : ':';
SEMICOLON           : ';';
ATAT                : '@@';
AT                  : '@';
DIV_OP              : 'div';
MOD_OP              : 'mod';

// --- Literals ---
CHAR_LITERAL        : '\'' ( EscapeSequence | ~['\\\r\n] ) '\'' ;
STRING              : '"' ( EscapeSequence | ~["\\\r\n] )*? '"'
                    | '\'' ( EscapeSequence | ~['\\\r\n] )*? '\''
                    ;
REAL                : (DIGIT+ '.' DIGIT* | '.' DIGIT+) ExpFragment?
                    | DIGIT+ ExpFragment
                    ;
INTEGER             : DecInteger | HexInteger ;

// --- Identifier ---
ID                  : LETTER (LETTER | DIGIT | '_' | '@')* ;

// --- Comments ---
LINE_COMMENT        : '|' ~[\r\n]* -> channel(HIDDEN);
DOC_COMMENT         : '#' ~[\r\n]* -> channel(HIDDEN);

// --- Whitespace ---
WS                  : [ \t\r\n]+ -> skip;

// --- Fragments ---
fragment DIGIT      : [0-9];
fragment HEX_DIGIT  : [0-9a-fA-F];
fragment LETTER     : [a-zA-Zа-яА-ЯёЁ];
fragment DecInteger : DIGIT+;
fragment HexInteger : '$' HEX_DIGIT+;
fragment ExpFragment: [eE] [+-]? DIGIT+;
fragment EscapeSequence
                    : '\\' [btnfr"'\\]
                    ;