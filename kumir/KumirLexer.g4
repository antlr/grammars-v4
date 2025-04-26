// KumirLexer.g4
// ANTLR v4 Lexer Grammar for the Kumir language.
// Source: Refined based on official documentation and extensive testing
//         against K.Y. Polyakov's examples. Developed collaboratively.
// Author: [Your Name/GitHub Handle]
// License: MIT License

lexer grammar KumirLexer;

// --- Keywords (Core Language) ---
// Keywords are case-insensitive (both lowercase and uppercase Cyrillic are matched).
MODULE              : ('модуль' | 'МОДУЛЬ');
ENDMODULE           : ('конец' WS 'модуля' | 'КОНЕЦ' WS 'МОДУЛЯ' | 'конецмодуля' | 'КОНЕЦМОДУЛЯ' | 'конец_модуля' | 'КОНЕЦ_МОДУЛЯ'); // Handles space, no space, underscore
ALG_HEADER          : ('алг' | 'АЛГ');
ALG_BEGIN           : ('нач' | 'НАЧ');
ALG_END             : ('кон' | 'КОН');
PRE_CONDITION       : ('дано' | 'ДАНО');
POST_CONDITION      : ('надо' | 'НАДО');
ASSERTION           : ('утв' | 'УТВ');
LOOP                : ('нц' | 'НЦ');
ENDLOOP_COND        : ('кц' WS 'при') | 'кц_при' | ('КЦ' WS 'ПРИ') | 'КЦ_ПРИ'; // Handles space and underscore for 'кц при'
ENDLOOP             : ('кц' | 'КЦ');
IF                  : ('если' | 'ЕСЛИ');
THEN                : ('то' | 'ТО');
ELSE                : ('иначе' | 'ИНАЧЕ');
FI                  : ('все' | 'ВСЕ'); // End of 'if' or 'switch' block
SWITCH              : ('выбор' | 'ВЫБОР');
CASE                : ('при' | 'ПРИ'); // Used in 'switch' and 'endloop conditional'
INPUT               : ('ввод' | 'ВВОД');
OUTPUT              : ('вывод' | 'ВЫВОД');
ASSIGN              : ':='; // Assignment operator
EXIT                : ('выход' | 'ВЫХОД');
PAUSE               : ('пауза' | 'ПАУЗА');
STOP                : ('стоп' | 'СТОП');
IMPORT              : ('использовать' | 'ИСПОЛЬЗОВАТЬ');
FOR                 : ('для' | 'ДЛЯ');
WHILE               : ('пока' | 'ПОКА');
TIMES               : ('раз' | 'РАЗ'); // Used in 'N times' loop
FROM                : ('от' | 'ОТ'); // Used in 'for' loop
TO                  : ('до' | 'ДО'); // Used in 'for' loop
STEP                : ('шаг' | 'ШАГ'); // Used in 'for' loop
NEWLINE_CONST       : ('нс' | 'НС'); // Newline constant for I/O
NOT                 : ('не' | 'НЕ'); // Logical NOT
AND                 : ('и' | 'И'); // Logical AND
OR                  : ('или' | 'ИЛИ'); // Logical OR
OUT_PARAM           : ('рез' | 'РЕЗ'); // Output parameter modifier
IN_PARAM            : ('арг' | 'АРГ'); // Input parameter modifier
INOUT_PARAM         : ('аргрез' | 'АРГРЕЗ' | 'арг' WS 'рез' | 'АРГ' WS 'РЕЗ' | 'арг_рез' | 'АРГ_РЕЗ'); // In/Out parameter modifier (various spellings)
RETURN_VALUE        : ('знач' | 'ЗНАЧ'); // Special variable for function return value

// --- Data Types ---
INTEGER_TYPE        : ('цел' | 'ЦЕЛ');
REAL_TYPE           : ('вещ' | 'ВЕЩ');
BOOLEAN_TYPE        : ('лог' | 'ЛОГ');
CHAR_TYPE           : ('сим' | 'СИМ');
STRING_TYPE         : ('лит' | 'ЛИТ');
TABLE_SUFFIX        : 'таб' | 'ТАБ'; // Suffix for tables (e.g., "цел таб")
// Actor-specific types
KOMPL_TYPE          : ('компл' | 'КОМПЛ'); // Complex numbers
COLOR_TYPE          : ('цвет' | 'ЦВЕТ');   // Color type (Painter actor)
SCANCODE_TYPE       : ('сканкод' | 'СКАНКОД'); // Key scancode (Keyboard actor)
FILE_TYPE           : ('файл' | 'ФАЙЛ');   // File type
// Explicit array/table types (handle variations with space, no space, underscore)
INTEGER_ARRAY_TYPE  : ('цел' WS? 'таб' | 'ЦЕЛ' WS? 'ТАБ' | 'цел_таб' | 'ЦЕЛ_ТАБ');
REAL_ARRAY_TYPE     : ('вещ' WS? 'таб' | 'ВЕЩ' WS? 'ТАБ' | 'вещ_таб' | 'ВЕЩ_ТАБ');
CHAR_ARRAY_TYPE     : ('сим' WS? 'таб' | 'СИМ' WS? 'ТАБ' | 'сим_таб' | 'СИМ_ТАБ');
STRING_ARRAY_TYPE   : ('лит' WS? 'таб' | 'ЛИТ' WS? 'ТАБ' | 'лит_таб' | 'ЛИТ_ТАБ');
BOOLEAN_ARRAY_TYPE  : ('лог' WS? 'таб' | 'ЛОГ' WS? 'ТАБ' | 'лог_таб' | 'ЛОГ_ТАБ');

// --- Constants ---
TRUE                : ('да' | 'ДА');
FALSE               : ('нет' | 'НЕТ');
// Color constants
PROZRACHNIY         : ('прозрачный' | 'ПРОЗРАЧНЫЙ');
BELIY               : ('белый' | 'БЕЛЫЙ');
CHERNIY             : ('чёрный' | 'черный' | 'ЧЁРНЫЙ' | 'ЧЕРНЫЙ');
SERIY               : ('серый' | 'СЕРЫЙ');
FIOLETOVIY          : ('фиолетовый' | 'ФИОЛЕТОВЫЙ');
SINIY               : ('синий' | 'СИНИЙ');
GOLUBOY             : ('голубой' | 'ГОЛУБОЙ');
ZELENIY             : ('зелёный' | 'зеленый' | 'ЗЕЛЁНЫЙ' | 'ЗЕЛЕНЫЙ');
ZHELTIY             : ('жёлтый' | 'желтый' | 'ЖЁЛТЫЙ' | 'ЖЕЛТЫЙ');
ORANZHEVIY          : ('оранжевый' | 'ОРАНЖЕВЫЙ');
KRASNIY             : ('красный' | 'КРАСНЫЙ');

// --- Operators ---
POWER               : '**'; // Power operator
GE                  : '>=' | '≥'; // Greater than or equal
LE                  : '<=' | '≤'; // Less than or equal
NE                  : '<>' | '≠'; // Not equal
PLUS                : '+';
MINUS               : '-';
MUL                 : '*';
DIV                 : '/';
EQ                  : '='; // Equal (used in comparisons and initialization)
LT                  : '<';
GT                  : '>';
LPAREN              : '('; // Left parenthesis
RPAREN              : ')'; // Right parenthesis
LBRACK              : '['; // Left square bracket (arrays)
RBRACK              : ']'; // Right square bracket (arrays)
LBRACE              : '{'; // Left curly brace (array literals)
RBRACE              : '}'; // Right curly brace (array literals)
COMMA               : ','; // Comma
COLON               : ':'; // Colon (array bounds, output format)
SEMICOLON           : ';'; // Semicolon (statement separator)
ATAT                : '@@'; // Double at-sign (special teacher functions)
AT                  : '@';  // At-sign (used in identifiers)

// --- Literals ---
// Character literal: single character in single quotes
CHAR_LITERAL        : '\'' ( EscapeSequence | ~['\\\r\n] ) '\'' ;
// String literal: sequence of characters in double or single quotes
STRING              : '"' ( EscapeSequence | ~["\\\r\n] )*? '"'
                    | '\'' ( EscapeSequence | ~['\\\r\n] )*? '\''
                    ;
// Real literal: with decimal point or in exponential form
REAL                : (DIGIT+ '.' DIGIT* | '.' DIGIT+) ExpFragment? // 123. , .5 , 123.45
                    | DIGIT+ ExpFragment                          // 123e4
                    ;
// Integer literal: decimal or hexadecimal (starts with $)
INTEGER             : DecInteger | HexInteger ;

// --- Identifier ---
// Variable, algorithm, module name, etc.
// Starts with a letter, followed by letters, digits, '_', or '@'.
ID                  : LETTER (LETTER | DIGIT | '_' | '@')* ;

// --- Comments ---
// Comments starting with '|' or '#' are ignored by the parser.
// The comment text is sent to the HIDDEN channel.
// Does NOT consume the trailing newline.
LINE_COMMENT        : '|' ~[\r\n]* -> channel(HIDDEN); // Single-line comment
DOC_COMMENT         : '#' ~[\r\n]* -> channel(HIDDEN); // Documentation comment

// --- Whitespace ---
// Spaces, tabs, newlines are skipped by the lexer.
WS                  : [ \t\r\n]+ -> skip;

// --- Fragments ---
// Fragments are helper rules for the lexer and do not produce tokens themselves.
fragment DIGIT      : [0-9];
fragment HEX_DIGIT  : [0-9a-fA-F];
// Letter (Latin or Cyrillic, including ё/Ё)
fragment LETTER     : [a-zA-Zа-яА-ЯёЁ];
fragment DecInteger : DIGIT+;
fragment HexInteger : '$' HEX_DIGIT+;
// Exponential part of a real number (e.g., e+10, E-5, е-3)
fragment ExpFragment: [eEеЕ] [+-]? DIGIT+;
// Escape sequences within strings/chars (e.g., \n, \t, \')
fragment EscapeSequence
                    : '\\' [btnfr"'\\] // \b, \t, \n, \f, \r, \", \', \\
                    ;
// End of KumirLexer.g4