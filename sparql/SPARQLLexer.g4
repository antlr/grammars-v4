/*
 * Copyright (c) 2023 by Bart Kiers
 *
 * The MIT license.
 *
 * Permission is hereby granted, free of charge, to any person
 * obtaining a copy of this software and associated documentation
 * files (the "Software"), to deal in the Software without
 * restriction, including without limitation the rights to use,
 * copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the
 * Software is furnished to do so, subject to the following
 * conditions:
 *
 * The above copyright notice and this permission notice shall be
 * included in all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
 * EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES
 * OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
 * NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
 * HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
 * WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
 * FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
 * OTHER DEALINGS IN THE SOFTWARE.
 *
 * (REVISION Last updated: 18 October 2023)
 */
lexer grammar SPARQLLexer;

options { caseInsensitive = true; }

SPACE : WS -> channel(HIDDEN);
COMMENT : '#' ~[\r\n]* -> channel(HIDDEN);

TRUE : 'TRUE';
FALSE : 'FALSE';
DISTINCT : 'DISTINCT';
NOT : 'NOT';
IN : 'IN';
STR : 'STR';
LANG : 'LANG';
LANGMATCHES : 'LANGMATCHES';
DATATYPE : 'DATATYPE';
BOUND : 'BOUND';
IRI : 'IRI';
URI : 'URI';
BNODE : 'BNODE';
RAND : 'RAND';
ABS : 'ABS';
CEIL : 'CEIL';
FLOOR : 'FLOOR';
ROUND : 'ROUND';
CONCAT : 'CONCAT';
STRLEN : 'STRLEN';
UCASE : 'UCASE';
LCASE : 'LCASE';
ENCODE_FOR_URI : 'ENCODE_FOR_URI';
CONTAINS : 'CONTAINS';
STRSTARTS : 'STRSTARTS';
STRENDS : 'STRENDS';
STRBEFORE : 'STRBEFORE';
STRAFTER : 'STRAFTER';
YEAR : 'YEAR';
MONTH : 'MONTH';
DAY : 'DAY';
HOURS : 'HOURS';
MINUTES : 'MINUTES';
SECONDS : 'SECONDS';
TIMEZONE : 'TIMEZONE';
TZ : 'TZ';
NOW : 'NOW';
UUID : 'UUID';
STRUUID : 'STRUUID';
MD5 : 'MD5';
SHA1 : 'SHA1';
SHA256 : 'SHA256';
SHA384 : 'SHA384';
SHA512 : 'SHA512';
COALESCE : 'COALESCE';
IF : 'IF';
STRLANG : 'STRLANG';
STRDT : 'STRDT';
SameTerm : 'SAMETERM';
IsIRI : 'ISIRI';
IsURI : 'ISURI';
IsBLANK : 'ISBLANK';
IsLITERAL : 'ISLITERAL';
IsNUMERIC : 'ISNUMERIC';
COUNT : 'COUNT';
SUM : 'SUM';
MIN : 'MIN';
MAX : 'MAX';
AVG : 'AVG';
SAMPLE : 'SAMPLE';
GROUP_CONCAT : 'GROUP_CONCAT';
SEPARATOR : 'SEPARATOR';
REGEX : 'REGEX';
SUBSTR : 'SUBSTR';
REPLACE : 'REPLACE';
EXISTS : 'EXISTS';
SELECT : 'SELECT';
REDUCED : 'REDUCED';
AS : 'AS';
WHERE : 'WHERE';
OPTIONAL : 'OPTIONAL';
GRAPH : 'GRAPH';
SERVICE : 'SERVICE';
SILENT : 'SILENT';
BIND : 'BIND';
VALUES : 'VALUES';
UNDEF : 'UNDEF';
A_ : 'A';
UNION : 'UNION';
MINUS : 'MINUS';
FILTER : 'FILTER';
GROUP : 'GROUP';
BY : 'BY';
HAVING : 'HAVING';
ASC : 'ASC';
DESC : 'DESC';
LIMIT : 'LIMIT';
OFFSET : 'OFFSET';
ORDER : 'ORDER';
DEFAULT : 'DEFAULT';
NAMED : 'NAMED';
ALL : 'ALL';
USING : 'USING';
INSERT : 'INSERT';
DELETE : 'DELETE';
WITH : 'WITH';
DATA : 'DATA';
COPY : 'COPY';
TO : 'TO';
MOVE : 'MOVE';
CREATE : 'CREATE';
LOAD : 'LOAD';
INTO : 'INTO';
CLEAR : 'CLEAR';
DROP : 'DROP';
BASE : 'BASE';
PREFIX : 'PREFIX';
CONSTRUCT : 'CONSTRUCT';
FROM : 'FROM';
DESCRIBE : 'DESCRIBE';
ASK : 'ASK';

CARET2 : '^^';
CARET : '^';
OPAR : '(';
CPAR : ')';
COMMA : ',';
OR : '||';
AND : '&&';
EQ : '=';
NEQ : '!=';
GT : '>';
LT : '<';
GTE : '>=';
LTE : '<=';
ADD : '+';
SUB : '-';
MUL : '*';
DIV : '/';
EXCL : '!';
SCOL : ';';
OBRACE : '{';
CBRACE : '}';
DOT : '.';
OBRACK : '[';
CBRACK : ']';
PIPE : '|';
QMARK : '?';

/// [139]    IRIREF : '<' ([^<>"{}|^`\]-[#x00-#x20])* '>'
IRIREF : '<' ~[<>"{}|^`\\\u0000-\u0020]* '>';

/// [140]    PNAME_NS : PN_PREFIX? ':'
PNAME_NS : PN_PREFIX? ':';

/// [141]    PNAME_LN : PNAME_NS PN_LOCAL
PNAME_LN : PNAME_NS PN_LOCAL;

/// [142]    BLANK_NODE_LABEL : '_:' ( PN_CHARS_U | [0-9] ) ((PN_CHARS|'.')* PN_CHARS)?
BLANK_NODE_LABEL : '_:' ( PN_CHARS_U | [0-9] ) ( ( PN_CHARS|'.' )* PN_CHARS )?;

/// [143]    VAR1 : '?' VARNAME
VAR1 : '?' VARNAME;

/// [144]    VAR2 : '$' VARNAME
VAR2 : '$' VARNAME;

/// [145]    LANGTAG : '@' [a-zA-Z]+ ('-' [a-zA-Z0-9]+)*
LANGTAG : '@' [A-Z]+ ( '-' [A-Z0-9]+ )*;

/// [146]    INTEGER : [0-9]+
INTEGER : [0-9]+;

/// [147]    DECIMAL : [0-9]* '.' [0-9]+
DECIMAL : [0-9]* '.' [0-9]+;

/// [148]    DOUBLE : [0-9]+ '.' [0-9]* EXPONENT | '.' ([0-9])+ EXPONENT | ([0-9])+ EXPONENT
DOUBLE
 : [0-9]+ '.' [0-9]* EXPONENT
 | '.' [0-9]+ EXPONENT
 | [0-9]+ EXPONENT
 ;

/// [149]    INTEGER_POSITIVE : '+' INTEGER
INTEGER_POSITIVE : '+' INTEGER;

/// [150]    DECIMAL_POSITIVE : '+' DECIMAL
DECIMAL_POSITIVE : '+' DECIMAL;

/// [151]    DOUBLE_POSITIVE : '+' DOUBLE
DOUBLE_POSITIVE : '+' DOUBLE;

/// [152]    INTEGER_NEGATIVE : '-' INTEGER
INTEGER_NEGATIVE : '-' INTEGER;

/// [153]    DECIMAL_NEGATIVE : '-' DECIMAL
DECIMAL_NEGATIVE : '-' DECIMAL;

/// [154]    DOUBLE_NEGATIVE : '-' DOUBLE
DOUBLE_NEGATIVE : '-' DOUBLE;

/// [156]    STRING_LITERAL1 : "'" ( ([^#x27#x5C#xA#xD]) | ECHAR )* "'"
STRING_LITERAL1 : '\'' ( ~[\u0027\u005C\u000A\u000D] | ECHAR )* '\'';

/// [157]    STRING_LITERAL2 : '"' ( ([^#x22#x5C#xA#xD]) | ECHAR )* '"'
STRING_LITERAL2 : '"' ( ~[\u0022\u005C\u000A\u000D] | ECHAR )* '"';

/// [158]    STRING_LITERAL_LONG1 : "'''" ( ( "'" | "''" )? ( [^'\] | ECHAR ) )* "'''"
STRING_LITERAL_LONG1 : '\'\'\'' ( ( '\'' | '\'\'' )? ( ~['\\] | ECHAR ) )* '\'\'\'';

/// [159]    STRING_LITERAL_LONG2 : '"""' ( ( '"' | '""' )? ( [^"\] | ECHAR ) )* '"""'
STRING_LITERAL_LONG2 : '"""' ( ( '"' | '""' )? ( ~["\\] | ECHAR ) )* '"""';

/// [161]    NIL : '(' WS* ')'
NIL : '(' WS* ')';

/// [163]    ANON : '[' WS* ']'
ANON : '[' WS* ']';

// Fall through rule
UNEXPECTED_CHAR : . ;

/// [155]    EXPONENT : [eE] [+-]? [0-9]+
fragment EXPONENT : 'E' [+-]? [0-9]+;

/// [160]    ECHAR : '\' [tbnrf\"']
fragment ECHAR options { caseInsensitive=false; } : '\\' [tbnrf"'];

/// [162]    WS : #x20 | #x9 | #xD | #xA
fragment WS : [\u0020\u0009\u000D\u000A];

/// [164]    PN_CHARS_BASE : [A-Z] | [a-z] | [#x00C0-#x00D6] | [#x00D8-#x00F6] | [#x00F8-#x02FF] | [#x0370-#x037D] | [#x037F-#x1FFF] | [#x200C-#x200D] | [#x2070-#x218F] | [#x2C00-#x2FEF] | [#x3001-#xD7FF] | [#xF900-#xFDCF] | [#xFDF0-#xFFFD] | [#x10000-#xEFFFF]
fragment PN_CHARS_BASE
 : [A-Z]
 | [ÀÁÂÃÄÅÆÇÈÉÊËÌÍÎÏÐÑÒÓÔÕÖØÙÚÛÜÝÞß]
 | [ĀĂĄĆĈĊČĎĐĒĔĖĘĚĜĞĠĢĤĦĨĪĬĮĲĴĶĸĹĻĽĿŁŃŅŇŉŊŌŎŐŒŔŖŘŚŜŞŠŢŤŦŨŪŬŮŰŲŴŶŸŹŻŽƀƁƂƄƆƇƉƊƋƍƎƏƐƑƓƔƕƖƗƘƚƛƜƝƞƟƠƢƤƦƧƩƪƫƬƮƯƱƲƳƵƷƸƺƻƼƾƿ]
 | [\u0370-\u037D]
 | [\u037F-\u1FFF]
 | [\u200C-\u200D]
 | [\u2070-\u218F]
 | [\u2C00-\u2FEF]
 | [\u3001-\uD7FF]
 | [\uF900-\uFDCF]
 | [\uFDF0-\uFFFD]
 | [\u{10000}-\u{EFFFF}]
 ;

/// [165]    PN_CHARS_U : PN_CHARS_BASE | '_'
fragment PN_CHARS_U : PN_CHARS_BASE | '_';

/// [166]    VARNAME : ( PN_CHARS_U | [0-9] ) ( PN_CHARS_U | [0-9] | #x00B7 | [#x0300-#x036F] | [#x203F-#x2040] )*
fragment VARNAME : ( PN_CHARS_U | [0-9] ) ( PN_CHARS_U | [0-9\u00B7\u0300-\u036F\u203F-\u2040] )*;

/// [167]    PN_CHARS : PN_CHARS_U | '-' | [0-9] | #x00B7 | [#x0300-#x036F] | [#x203F-#x2040]
fragment PN_CHARS
 : PN_CHARS_U
 | [\-0-9\u00B7\u0300-\u036F\u203F-\u2040]
 ;

/// [168]    PN_PREFIX : PN_CHARS_BASE ((PN_CHARS|'.')* PN_CHARS)?
fragment PN_PREFIX : PN_CHARS_BASE ((PN_CHARS|'.')* PN_CHARS)?;

/// [169]    PN_LOCAL : (PN_CHARS_U | ':' | [0-9] | PLX ) ((PN_CHARS | '.' | ':' | PLX)* (PN_CHARS | ':' | PLX) )?
fragment PN_LOCAL : (PN_CHARS_U | ':' | [0-9] | PLX ) ((PN_CHARS | '.' | ':' | PLX)* (PN_CHARS | ':' | PLX) )?;

/// [170]    PLX : PERCENT | PN_LOCAL_ESC
fragment PLX : PERCENT | PN_LOCAL_ESC;

/// [171]    PERCENT : '%' HEX HEX
fragment PERCENT : '%' HEX HEX;

/// [172]    HEX : [0-9] | [A-F] | [a-f]
fragment HEX : [0-9A-F];

/// [173]    PN_LOCAL_ESC : '\' ( '_' | '~' | '.' | '-' | '!' | '$' | '&' | "'" | '(' | ')' | '*' | '+' | ',' | ';' | '=' | '/' | '?' | '#' | '@' | '%' )
fragment PN_LOCAL_ESC : '\\' [_~.\-!$&'()*+,;=/?#@%];
