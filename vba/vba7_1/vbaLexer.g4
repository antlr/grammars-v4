// $antlr-format alignTrailingComments true, columnLimit 150, minEmptyLines 1, maxEmptyLinesToKeep 1, reflowComments false, useTab false
// $antlr-format allowShortRulesOnASingleLine false, allowShortBlocksOnASingleLine true, alignSemicolons hanging, alignColons hanging

lexer grammar vbaLexer;

options {
    caseInsensitive = true;
}

// keywords
ABS
    : 'ABS'
    ;

ACCESS
    : 'ACCESS'
    ;

ADDRESSOF
    : 'ADDRESSOF'
    ;

ALIAS
    : 'ALIAS'
    ;

AND
    : 'AND'
    ;

ANY
    : 'ANY'
    ;

ATTRIBUTE
    : 'ATTRIBUTE'
    ;

APPEND
    : 'APPEND'
    ;

ARRAY
    : 'ARRAY'
    ;

AS
    : 'AS'
    ;

ASSERT
    : 'ASSERT'
    ;

BASE
    : 'BASE'
    ;

BEGIN
    : 'BEGIN'
    ;
BEGINPROPERTY
    : 'BEGINPROPERTY'
    ;

BINARY
    : 'BINARY'
    ;

BOOLEAN
    : 'BOOLEAN'
    ;

BOOLEAN_B
    : '[BOOLEAN]'
    ;

BYVAL
    : 'BYVAL'
    ;

BYREF
    : 'BYREF'
    ;

BYTE
    : 'BYTE'
    ;

BYTE_B
    : '[BYTE]'
    ;

CALL
    : 'CALL'
    ;

CASE
    : 'CASE'
    ;

CBOOL
    : 'CBOOL'
    ;

CBYTE
    : 'CBYTE'
    ;

CCUR
    : 'CCUR'
    ;

CDATE
    : 'CDATE'
    ;

CDBL
    : 'CDBL'
    ;

CDEC
    : 'CDEC'
    ;

CDECL
    : 'CDECL'
    ;

CHDIR
    : 'CHDIR'
    ;

CHDRIVE
    : 'CHDRIVE'
    ;

CINT
    : 'CINT'
    ;

CIRCLE
    : 'CIRCLE'
    ;

CLASS
    : 'CLASS'
    ;

CLASS_INITIALIZE
    : 'CLASS_INITIALIZE'
    ;

CLASS_TERMINATE
    : 'CLASS_TERMINATE'
    ;

CLNG
    : 'CLNG'
    ;

CLNGLNG
    : 'CLNGLNG'
    ;

CLNGPTR
    : 'CLNGPTR'
    ;

CLOSE
    : 'CLOSE'
    ;

COMPARE
    : 'COMPARE'
    ;

CONST
    : 'CONST'
    ;

CSNG
    : 'CSNG'
    ;

CSTR
    : 'CSTR'
    ;

CVAR
    : 'CVAR'
    ;

CVERR
    : 'CVERR'
    ;

CURRENCY
    : 'CURRENCY'
    ;

CURRENCY_B
    : '[CURRENCY]'
    ;

DATE
    : 'DATE'
    ;

DATE_B
    : '[DATE]'
    ;

DEBUG
    : 'DEBUG'
    ;

DECLARE
    : 'DECLARE'
    ;

DECIMAL
    : 'DECIMAL'
    ;

DEFBOOL
    : 'DEFBOOL'
    ;

DEFBYTE
    : 'DEFBYTE'
    ;

DEFCUR
    : 'DEFCUR'
    ;

DEFDATE
    : 'DEFDATE'
    ;

DEFDBL
    : 'DEFDBL'
    ;

DEFDEC
    : 'DEFDEC'
    ;

DEFINT
    : 'DEFINT'
    ;

DEFLNG
    : 'DEFLNG'
    ;

DEFLNGLNG
    : 'DEFLNGLNG'
    ;

DEFLNGPTR
    : 'DEFLNGPTR'
    ;

DEFOBJ
    : 'DEFOBJ'
    ;

DEFSNG
    : 'DEFSNG'
    ;

DEFSTR
    : 'DEFSTR'
    ;

DEFVAR
    : 'DEFVAR'
    ;

DIM
    : 'DIM'
    ;

DO
    : 'DO'
    ;

DOEVENTS
    : 'DOEVENTS'
    ;

DOUBLE
    : 'DOUBLE'
    ;

DOUBLE_B
    : '[DOUBLE]'
    ;

EACH
    : 'EACH'
    ;

ELSE
    : 'ELSE'
    ;

ELSEIF
    : 'ELSEIF'
    ;

EMPTY_X
    : 'EMPTY'
    ;

ENDIF
    : 'ENDIF'
    ;

END
    : 'END'
    ;

ENDPROPERTY
    : 'ENDPROPERTY'
    ;

ENUM
    : 'ENUM'
    ;

EQV
    : 'EQV'
    ;

ERASE
    : 'ERASE'
    ;

ERROR
    : 'ERROR'
    ;

EVENT
    : 'EVENT'
    ;

EXIT
    : 'EXIT'
    ;

EXPLICIT
    : 'EXPLICIT'
    ;

FALSE
    : 'FALSE'
    ;

FIX
    : 'FIX'
    ;

FRIEND
    : 'FRIEND'
    ;

FOR
    : 'FOR'
    ;

FUNCTION
    : 'FUNCTION'
    ;

GET
    : 'GET'
    ;

GLOBAL
    : 'GLOBAL'
    ;

GO
    : 'GO'
    ;

GOSUB
    : 'GOSUB'
    ;

GOTO
    : 'GOTO'
    ;

IF
    : 'IF'
    ;

IMP
    : 'IMP'
    ;

IMPLEMENTS
    : 'IMPLEMENTS'
    ;

IN
    : 'IN'
    ;

INPUT
    : 'INPUT'
    ;

INPUTB
    : 'INPUTB'
    ;

INT
    : 'INT'
    ;

IS
    : 'IS'
    ;

INTEGER
    : 'INTEGER'
    ;

INTEGER_B
    : '[INTEGER]'
    ;

KILL
    : 'KILL'
    ;

LBOUND
    : 'LBOUND'
    ;

LEN
    : 'LEN'
    ;

LENB
    : 'LENB'
    ;

LET
    : 'LET'
    ;

LIB
    : 'LIB'
    ;

LIKE
    : 'LIKE'
    ;

LINE
    : 'LINE'
    ;

LINEINPUT
    : 'LINEINPUT'
    ;

LOCK
    : 'LOCK'
    ;

LONG
    : 'LONG'
    ;

LONG_B
    : '[LONG]'
    ;

LONGLONG
    : 'LONGLONG'
    ;

LONGLONG_B
    : '[LONGLONG]'
    ;

LONGPTR
    : 'LONGPTR'
    ;

LONGPTR_B
    : '[LONGPTR]'
    ;

LOOP
    : 'LOOP'
    ;

LSET
    : 'LSET'
    ;

ME
    : 'ME'
    ;

MID
    : 'MID'
    ;

MIDB
    : 'MIDB'
    ;

MID_D
    : 'MID$'
    ;

MIDB_D
    : 'MIDB$'
    ;

MOD
    : 'MOD'
    ;

MODULE
    : 'MODULE'
    ;

NEXT
    : 'NEXT'
    ;

NEW
    : 'NEW'
    ;

NOT
    : 'NOT'
    ;

NOTHING
    : 'NOTHING'
    ;

NULL_
    : 'NULL'
    ;

OBJECT
    : 'OBJECT'
    ;

OBJECT_B
    : '[OBJECT]'
    ;

ON
    : 'ON'
    ;

OPEN
    : 'OPEN'
    ;

OPTION
    : 'OPTION'
    ;

OPTIONAL
    : 'OPTIONAL'
    ;

OR
    : 'OR'
    ;

OUTPUT
    : 'OUTPUT'
    ;

PARAMARRAY
    : 'PARAMARRAY'
    ;

PRESERVE
    : 'PRESERVE'
    ;

PRINT
    : 'PRINT'
    ;

PRIVATE
    : 'PRIVATE'
    ;

PROPERTY
    : 'PROPERTY'
    ;

PSET
    : 'PSET'
    ;

PTRSAFE
    : 'PTRSAFE'
    ;

PUBLIC
    : 'PUBLIC'
    ;

PUT
    : 'PUT'
    ;

RANDOM
    : 'RANDOM'
    ;

RAISEEVENT
    : 'RAISEEVENT'
    ;

READ
    : 'READ'
    ;

REDIM
    : 'REDIM'
    ;

REM
    : 'REM'
    ;

RESET
    : 'RESET'
    ;

RESUME
    : 'RESUME'
    ;

RETURN
    : 'RETURN'
    ;

RSET
    : 'RSET'
    ;
 
SCALE
    : 'SCALE'
    ;

SEEK
    : 'SEEK'
    ;

SELECT
    : 'SELECT'
    ;

SET
    : 'SET'
    ;

SGN
    : 'SGN'
    ;

SHARED
    : 'SHARED'
    ;

SINGLE
    : 'SINGLE'
    ;

SINGLE_B
    : '[SINGLE]'
    ;

SPC
    : 'SPC'
    ;

STATIC
    : 'STATIC'
    ;

STEP
    : 'STEP'
    ;

STOP
    : 'STOP'
    ;

STRING
    : 'STRING'
    ;

STRING_B
    : '[STRING]'
    ;

SUB
    : 'SUB'
    ;

TAB
    : 'TAB'
    ;

TEXT
    : 'TEXT'
    ;

THEN
    : 'THEN'
    ;

TO
    : 'TO'
    ;

TRUE
    : 'TRUE'
    ;

TYPE
    : 'TYPE'
    ;

TYPEOF
    : 'TYPEOF'
    ;

UBOUND
    : 'UBOUND'
    ;

UNLOCK
    : 'UNLOCK'
    ;

UNTIL
    : 'UNTIL'
    ;

VB_BASE
    : 'VB_BASE'
    ;

VB_CONTROL
    : 'VB_CONTROL'
    ;

VB_CREATABLE
    : 'VB_CREATABLE'
    ;

VB_CUSTOMIZABLE
    : 'VB_CUSTOMIZABLE'
    ;

VB_DESCRIPTION
    : 'VB_DESCRIPTION'
    ;

VB_EXPOSED
    : 'VB_EXPOSED'
    ;

VB_EXT_KEY 
    : 'VB_EXT_KEY '
    ;

VB_GLOBALNAMESPACE
    : 'VB_GLOBALNAMESPACE'
    ;

VB_HELPID
    : 'VB_HELPID'
    ;

VB_INVOKE_FUNC
    : 'VB_INVOKE_FUNC'
    ;

VB_INVOKE_PROPERTY 
    : 'VB_INVOKE_PROPERTY '
    ;

VB_INVOKE_PROPERTYPUT
    : 'VB_INVOKE_PROPERTYPUT'
    ;

VB_INVOKE_PROPERTYPUTREF
    : 'VB_INVOKE_PROPERTYPUTREF'
    ;

VB_MEMBERFLAGS
    : 'VB_MEMBERFLAGS'
    ;

VB_NAME
    : 'VB_NAME'
    ;

VB_PREDECLAREDID
    : 'VB_PREDECLAREDID'
    ;

VB_PROCDATA
    : 'VB_PROCDATA'
    ;

VB_TEMPLATEDERIVED
    : 'VB_TEMPLATEDERIVED'
    ;

VB_USERMEMID
    : 'VB_USERMEMID'
    ;

VB_VARDESCRIPTION
    : 'VB_VARDESCRIPTION'
    ;

VB_VARHELPID
    : 'VB_VARHELPID'
    ;

VB_VARMEMBERFLAGS
    : 'VB_VARMEMBERFLAGS'
    ;

VB_VARPROCDATA 
    : 'VB_VARPROCDATA '
    ;

VB_VARUSERMEMID
    : 'VB_VARUSERMEMID'
    ;

VARIANT
    : 'VARIANT'
    ;

VARIANT_B
    : '[VARIANT]'
    ;

VERSION
    : 'VERSION'
    ;

WEND
    : 'WEND'
    ;

WHILE
    : 'WHILE'
    ;

WIDTH
    : 'WIDTH'
    ;

WITH
    : 'WITH'
    ;

WITHEVENTS
    : 'WITHEVENTS'
    ;

WRITE
    : 'WRITE'
    ;

XOR
    : 'XOR'
    ;

// Standard Library functions, subs, and properties
// should these be removed?
APPACTIVATE
    : 'APPACTIVATE'
    ;

COLLECTION
    : 'COLLECTION'
    ;

DATABASE
    : 'DATABASE'
    ;
 
DELETESETTING
    : 'DELETESETTING'
    ;

FILECOPY
    : 'FILECOPY'
    ;

MKDIR
    : 'MKDIR'
    ;

NAME
    : 'NAME'
    ;

RANDOMIZE
    : 'RANDOMIZE'
    ;

RMDIR
    : 'RMDIR'
    ;

SAVEPICTURE
    : 'SAVEPICTURE'
    ;

SAVESETTING
    : 'SAVESETTING'
    ;

SENDKEYS
    : 'SENDKEYS'
    ;

SETATTR
    : 'SETATTR'
    ;

TIME
    : 'TIME'
    ;

LOAD
    : 'LOAD'
    ;

UNLOAD
    : 'UNLOAD'
    ;

// symbols
AMPERSAND
    : '&'
    ;

ASPERAND
    : '@'
    ;

ASSIGN
    : ':='
    ;

COMMA
    : ','
    ;

DIV
    : '\\'
    | '/'
    ;
Dollar
    : '$'
    ;
EQ
    : '='
    ;
EXCLAM
    : '!'
    ;
GEQ
    : '>='
    | '=>'
    ;

GT
    : '>'
    ;

HASH
    : '#'
    ;
LEQ
    : '<='
    | '=<'
    ;

LPAREN
    : '('
    ;

LT
    : '<'
    ;

MINUS
    : '-'
    ;

MINUS_EQ
    : '-='
    ;

MULT
    : '*'
    ;

NEQ
    : '<>'
    | '><'
    ;

PERCENT
    : '%'
    ;

PERIOD
    : '.'
    ;

PLUS
    : '+'
    ;

PLUS_EQ
    : '+='
    ;

POW
    : '^'
    ;

RPAREN
    : ')'
    ;

SEMICOLON
    : ';'
    ;

L_SQUARE_BRACKET
    : '['
    ;

R_SQUARE_BRACKET
    : ']'
    ;

// literals
fragment BLOCK
    : HEXDIGIT HEXDIGIT HEXDIGIT HEXDIGIT
    ;

GUID
    : '{' BLOCK BLOCK MINUS BLOCK MINUS BLOCK MINUS BLOCK MINUS BLOCK BLOCK BLOCK '}'
    ;

STRINGLITERAL
    : '"' (~["\r\n] | '""')* '"'
    ;

INTEGERLITERAL
    : (DIGIT DIGIT*
    | '&H' [0-9A-F]+
    | '&' [O]? [0-7]+) [%&^]?
    ;

FLOATLITERAL
    : FLOATINGPOINTLITERAL [!#@]?
    | DECIMALLITERAL [!#@]
    ;

fragment FLOATINGPOINTLITERAL
    : DECIMALLITERAL [DE] [+-]? DECIMALLITERAL
    | DECIMALLITERAL '.' DECIMALLITERAL? ([DE] [+-]? DECIMALLITERAL)?
    | '.' DECIMALLITERAL ([DE] [+-]? DECIMALLITERAL)?
    ;

fragment DECIMALLITERAL
    : DIGIT DIGIT*
    ;

DATELITERAL
    : '#' DATEORTIME '#'
    ;

fragment DATEORTIME
    : DATEVALUE WS+ TIMEVALUE
    | DATEVALUE
    | TIMEVALUE
    ;

fragment DATEVALUE
    : DATEVALUEPART DATESEPARATOR DATEVALUEPART (DATESEPARATOR DATEVALUEPART)?
    ;

fragment DATEVALUEPART
    : DIGIT+
    | MONTHNAME
    ;

fragment DATESEPARATOR
    : WS+
    | WS? [/,-] WS?
    ;

fragment MONTHNAME
    : ENGLISHMONTHNAME
    | ENGLISHMONTHABBREVIATION
    ;

fragment ENGLISHMONTHNAME
    : 'JANUARY'
    | 'FEBRUARY'
    | 'MARCH'
    | 'APRIL'
    | 'MAY'
    | 'JUNE'
    | 'JULY'
    | 'AUGUST'
    | 'SEPTEMBER'
    | 'OCTOBER'
    | 'NOVEMBER'
    | 'DECEMBER'
    ;

// May has intentionally been left out
fragment ENGLISHMONTHABBREVIATION
    : 'JAN'
    | 'FEB'
    | 'MAR'
    | 'APR'
    | 'JUN'
    | 'JUL'
    | 'AUG'
    | 'SEP'
    | 'OCT'
    | 'NOV'
    | 'DEC'
    ;

fragment TIMEVALUE
    : DIGIT+ AMPM
    | DIGIT+ TIMESEPARATOR DIGIT+ (TIMESEPARATOR DIGIT+)? AMPM?
    ;

fragment TIMESEPARATOR
    : WS? (':' | '.') WS?
    ;

fragment AMPM
    : WS? ('AM' | 'PM' | 'A' | 'P')
    ;

FILEOFFSET
    : '$'? STRINGLITERAL ':' HEXDIGIT+
    ;
// whitespace, line breaks, comments, ...
LINE_CONTINUATION
    : WS UNDERSCORE WS? '\r'? '\n'
    ;

NEWLINE
    : [\r\n\u2028\u2029]+
    ;

REMCOMMENT
    : COLON? REM WS (LINE_CONTINUATION | ~[\r\n\u2028\u2029])*
    ;

COMMENT
    : SINGLEQUOTE (LINE_CONTINUATION | ~[\r\n\u2028\u2029])*
    ;

SINGLEQUOTE
    : '\''
    ;

COLON
    : ':'
    ;

UNDERSCORE
    : '_'
    ;

WS
    : ([ \t\u0019\u3000])+
    ;

MACRO_LINE
    : (WS? '#IF' ~[\r\n\u2028\u2029]* THEN COMMENT?
    | WS? '#ELSEIF' ~[\r\n\u2028\u2029]* THEN COMMENT?
    | WS? '#ELSE' COMMENT?
    | WS? ('#END If'|'#endif') COMMENT?) -> channel(HIDDEN)
    ;

// identifier
IDENTIFIER
    : [A-Z][A-Z0-9_]*
    ;

FOREIGN_NAME
    : '[' ~[\r\n\u2028\u2029]* ']'
    ;

// letters
fragment LETTER
    : [A-Z_\p{L}]
    ;

fragment DIGIT
    : [0-9]
    ;

fragment HEXDIGIT
    : [A-F0-9]
    ;

fragment LETTERORDIGIT
    : [A-Z0-9_\p{L}]
    ;
