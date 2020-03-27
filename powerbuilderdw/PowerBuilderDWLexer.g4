

lexer grammar PowerBuilderDWLexer;

// Keywords
TABLE:              'TABLE';
COLUMN:             'COLUMN';
RETRIEVE:           'RETRIEVE';
PBSELECT:           'PBSELECT';
VERSION:            'VERSION';
ARGUMENTS:          'ARGUMENTS';
SORT:               'SORT';
ANY:                'ANY';
BLOB:               'BLOB';
BOOLEAN:            'BOOLEAN';
BYTE:               'BYTE';
CHARACTER:          'CHARACTER';
CHAR:               'CHAR';
DATE_TYPE:          'DATE';
DATETIME:           'DATETIME';
DECIMAL:            'DECIMAL';
DEC:                'DEC';
DOUBLE:             'DOUBLE';
INTEGER:            'INTEGER';
INT:                'INT';
LONG:               'LONG';
LONGLONG:           'LONGLONG';
REAL:               'REAL';
STRING:             'STRING';
TIME_TYPE:          'TIME';
UNSIGNEDINTEGER:    'UNSIGNEDINTEGER';
UINT:               'UINT';
UNSIGNEDLONG:       'UNSIGNEDLONG';
ULONG:              'ULONG';
WINDOW:             'WINDOW';
TRUE:               'TRUE';
FALSE:              'FALSE';
GLOBAL:             'GLOBAL';
SHARED:             'SHARED';
END:                'END';
INDIRECT :          'INDIRECT';
VARIABLES:          'VARIABLES';
FORWARD:            'FORWARD';
PUBLIC:             'PUBLIC';
PRIVATE:            'PRIVATE';
FUNCTION:           'FUNCTION';
SUBROUTINE:         'SUBROUTINE';
READONLY:           'READONLY';
PROTOTYPES:         'PROTOTYPES';
TYPE:               'TYPE';
ON:                 'ON';
TO:                 'TO';
FROM:               'FROM';
REF:                'REF';
NULL:               'NULL';
UPDATE:             'UPDATE';
CASE:               'CASE';
DYNAMIC:            'DYNAMIC';
WITHIN:             'WITHIN';
PRIVATEWRITE:       'PRIVATEWRITE';
PROTECTED:          'PROTECTED';
PRIVATEREAD:        'PRIVATEREAD';
PROTECTEDREAD:      'PROTECTEDREAD';
PROTECTEDWRITE:     'PROTECTEDWRITE';
LOCAL:              'LOCAL';
EVENT:              'EVENT';
OPEN:               'OPEN';
GOTO:               'GOTO';
ELSE:               'ELSE';
IF:                 'IF';
THEN:               'THEN';
ELSEIF:             'ELSEIF';
TRY:                'TRY';
EXIT:               'EXIT';
CHOOSE:             'CHOOSE';
IS:                 'IS';
CONTINUE:           'CONTINUE';
DO:                 'DO';
WHILE:              'WHILE';
FOR:                'FOR';
CLOSE:              'CLOSE';
NEXT:               'NEXT';
LOOP:               'LOOP';
UNTIL:              'UNTIL';
STEP:               'STEP';
CATCH:              'CATCH';
FINALLY:            'FINALLY';
THROW:              'THROW';
RELEASE:            'RELEASE';
CREATE:             'CREATE';
DESTROY:            'DESTROY';
USING:              'USING';
POST:               'POST';
TRIGGER:            'TRIGGER';
SELECT:             'SELECT';
DELETE:             'DELETE';
INSERT:             'INSERT';
DESCRIBE:           'DESCRIBE';
RETURN:             'RETURN';
OR:                 'OR';
AND:                'AND';
NOT:                'NOT';
CALL:               'CALL';
HALT:               'HALT';
SUPER:              'SUPER';
LIBRARY:            'LIBRARY';
SYSTEM:             'SYSTEM';
RPCFUNC:            'RPCFUNC';
ALIAS:              'ALIAS';
THROWS:             'THROWS';
AUTOINSTANTIATE:    'AUTOINSTANTIATE';
DESCRIPTOR:         'DESCRIPTOR';

// Operators

EQ:                 '=';
GT:                 '>';
GTE:                '>=';
LT:                 '<';
LTE:                '<=';
GTLT:               '<>';
PLUS:               '+';
MINUS:              '-';
PLUSEQ:             '+=';
MINUSEQ:            '-=';
COLONCOLON:         '::';
MULT:               '*';
DIV:                '/';
MULTEQ:             '*=';
DIVEQ:              '/=';
CARAT:              '^';
LCURLY:             '{';
RCURLY:             '}';
LBRACE:             '[';
RBRACE:             ']';
BRACES:             '[]';
TICK:               '`';
DQUOTED_STRING:     '"' ('~~' | ~'"' | '~"')* '"';
QUOTED_STRING:      '\'' (~'\'' | '~\'')* '\'';
COMMA:              ',';
SEMI:               ';';
LPAREN:             '(';
RPAREN:             ')';
COLON:              ':';
DQUOTE:             '"';
TQ:                 '???';
DOUBLE_PIPE:        '||';
DOTDOTDOT:          '...';

// Literals

NUMBER:             (NUM '.' NUM | '.' NUM | NUM) ('E' ('+' | '-')? NUM)? ('D' | 'F')?;
DOT:                '.';
DATE:               DIGIT DIGIT DIGIT DIGIT '-' DIGIT DIGIT '-' DIGIT DIGIT;
TIME:               DIGIT DIGIT ':' DIGIT DIGIT ':' DIGIT DIGIT (':' DIGIT DIGIT DIGIT DIGIT DIGIT DIGIT)?;
BINDPAR:            ':' ID_PARTS;
EXPORT_HEADER:      '$' LETTER ((LETTER | DIGIT | '-' | '#' | '%' | '_'))* '$' (LETTER | DIGIT | '.' | ' ' | '_')+ ~[\r\n];
ENUM:               ID_PARTS '!';
ID:                 ID_PARTS;
RET_LIT:                 RETRIEVE EQ DQUOTED_STRING;
ARGS_LIT:             ARGUMENTS EQ LPAREN LPAREN .+? RPAREN RPAREN   ;
SORT_LIT:                SORT EQ DQUOTED_STRING;



// Hidden

LINE_CONTINUATION:  '&' WS* [\r\n] -> channel(HIDDEN);
SL_COMMENT:         '//' ~ [\r\n]* -> channel(HIDDEN);
ML_COMMENT:         '/*' .*? '*/'  -> channel(HIDDEN);
WS:                 [ \t\r\n]+     -> channel(HIDDEN);

// Fragments

fragment ID_PARTS
   : [A-Z] ([A-Z] | DIGIT | '-' | '$' | '#' | '%' | '_')*
   ;

fragment NUM
   : DIGIT+
   ;

fragment DIGIT
   : '0' .. '9'
   ;
   
fragment LETTER
   : 'A' .. 'Z'
   ;