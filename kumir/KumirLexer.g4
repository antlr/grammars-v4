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
MODULE              : ('модуль' | 'исп');
ENDMODULE           : 'кон' (WS_FRAGMENT | '_') 'исп'
                    | 'конец' (WS_FRAGMENT | '_')? 'модуля'
                    ;
ALG_HEADER          : 'алг';
ALG_BEGIN           : 'нач';
ALG_END             : 'кон';
PRE_CONDITION       : 'дано';
POST_CONDITION      : 'надо';
ASSERTION           : 'утв';
LOOP                : 'нц';
ENDLOOP_COND        : 'кц' (WS_FRAGMENT | '_')? 'при';
ENDLOOP             : 'кц';
IF                  : 'если';
THEN                : 'то';
ELSE                : 'иначе';
FI                  : ('все' | 'всё');
SWITCH              : 'выбор';
CASE                : 'при';
INPUT               : ('ввод' | 'фввод');
OUTPUT              : ('вывод' | 'фвывод');
ASSIGN              : ':=';
EXIT                : 'выход';
PAUSE               : 'пауза';
STOP                : 'стоп';
IMPORT              : 'использовать';
FOR                 : 'для';
WHILE               : 'пока';
TIMES               : ('раз' | 'раза');
FROM                : 'от';
TO                  : 'до';
STEP                : 'шаг';
NEWLINE_CONST       : 'нс';
NOT                 : 'не';
AND                 : 'и';
OR                  : 'или';
OUT_PARAM           : 'рез';
IN_PARAM            : 'арг';
INOUT_PARAM         : 'арг' (WS_FRAGMENT | '_')? 'рез';
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
INTEGER_ARRAY_TYPE  : 'цел' (WS_FRAGMENT | '_')? 'таб';
REAL_ARRAY_TYPE     : 'вещ' (WS_FRAGMENT | '_')? 'таб';
CHAR_ARRAY_TYPE     : 'сим' (WS_FRAGMENT | '_')? 'таб';
STRING_ARRAY_TYPE   : 'лит' (WS_FRAGMENT | '_')? 'таб';
BOOLEAN_ARRAY_TYPE  : 'лог' (WS_FRAGMENT | '_')? 'таб';

// --- Constants ---
TRUE                : 'да';
FALSE               : 'нет';
// Color constants
PROZRACHNIY         : 'прозрачный';
BELIY               : 'белый';
CHERNIY             : 'ч' E_OR_YO 'рный';
SERIY               : 'серый';
FIOLETOVIY          : 'фиолетовый';
SINIY               : 'синий';
GOLUBOY             : 'голубой';
ZELENIY             : 'зел' E_OR_YO 'ный';
ZHELTIY             : 'ж' E_OR_YO 'лтый';
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
// DIV_OP              : 'div';
// MOD_OP              : 'mod';

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
WS                  : WS_FRAGMENT+ -> skip;

// --- Fragments ---
fragment WS_FRAGMENT: [ \t\r\n]+;
fragment E_OR_YO    : 'ё' | 'е';
fragment DIGIT      : [0-9];
fragment HEX_DIGIT  : [0-9a-f];
fragment LETTER     : [a-zа-яё];
fragment DecInteger : DIGIT+;
fragment HexInteger : '$' HEX_DIGIT+;
fragment ExpFragment: [e] [+-]? DIGIT+;
fragment EscapeSequence
                    : '\\' [btnfr"'\\]
                    ;