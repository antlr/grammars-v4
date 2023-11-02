/* MIT License
 * Author: Ken Domino
 * Date: Nov 2, 2023
 */
grammar gvpr;

preds: pred* EOF;

pred
    : 'BEGIN' ('{' (program | expr) '}')?
    | 'BEG_G' ('{' (program | expr) '}')?
    | 'N' ('[' expr ']')? ('{' (program | expr) '}')?
    | 'E' ('[' expr ']')? ('{' (program | expr) '}')?
    | 'END_G' ('{' (program | expr) '}')?
    | 'END' ('{' (program | expr) '}')?
    ;

IntegerConstant
    : DecimalConstant IntegerSuffix?
    | OctalConstant IntegerSuffix?
    | HexadecimalConstant IntegerSuffix?
    | BinaryConstant
    ;

FloatingConstant
    :   DecimalFloatingConstant
    |   HexadecimalFloatingConstant
    ;

CharacterConstant
    :   '\'' CCharSequence '\''
    |   'L\'' CCharSequence '\''
    |   'u\'' CCharSequence '\''
    |   'U\'' CCharSequence '\''
    ;

DigitSequence
    :   Digit+
    ;

program
    : statement_list? action_list?
    ;

action_list
    : action_+
    ;

action_
    : label ':' statement_list?
    ;

statement_list
    : statement+
    ;

statement
    : '{' statement_list? '}'
    | expr ';'
    | static? declare dcl_list ';'
    | static? declare fdcl_item
    | IF '(' expr ')' statement else_?
    | FOR '(' variable ')' statement
    | FOR '(' expr? ';' expr? ';' expr? ')' statement
    | ITERATER '(' variable ')' statement
    | UNSET '(' ID ')'
    | UNSET '(' ID ',' expr  ')'
    | WHILE '(' expr ')' statement
    | SWITCH '(' expr ')' '{' switch_list '}'
    | BREAK expr? ';'
    | CONTINUE expr? ';'
    | RETURN expr? ';'
    ;

switch_list
    : /* empty */
    | switch_list switch_item
    ;

switch_item
    : case_list statement_list?
    ;

case_list
    : case_item
    | case_list case_item
    ;

case_item
    : CASE constant ':'
    | DEFAULT ':'
    ;

static
    : STATIC
    ;

dcl_list
    : dcl_item (',' dcl_item)*
    ;

dcl_item
    : dcl_name array initialize_?
    ;

fdcl_item
    : dcl_name finitialize_
    ;

dcl_name
    : ID
    ;

name
    : ID
    ;

else_
    : ELSE statement
    ;

expr
    : '(' expr ')'
    | '(' declare ')' expr
    | (INC|DEC) variable
    | variable (INC|DEC)
    | ('!' expr | '#' ID | '~' expr | '-' expr | '+' expr | '&' variable)
    | expr ('*'|'/'|'%') expr
    | expr (('+'|'-') expr | IN_OP ID)
    | expr (LSH|RSH) expr
    | expr ('<'|'>'|LE|GE) expr
    | expr (EQ|NE) expr
    | expr '&' expr
    | expr '^' expr
    | expr '|' expr
    | expr AND expr
    | expr OR expr
    | <assoc=right> expr '?' expr ':' expr
    | expr ',' expr
    | array_ '[' args ']'
    | function '(' args ')'
    | GSUB '(' args ')'
    | SUB '(' args ')'
    | SUBSTR '(' args ')'
    | splitop '(' expr ',' ID ')'
    | splitop '(' expr ',' ID ',' expr ')'
    | EXIT '(' expr ')'
    | RAND '(' ')'
    | SRAND '(' ')'
    | SRAND '(' expr ')'
    | PROCEDURE '(' args ')'
    | print_ '(' args ')'
    | scan '(' args ')'
    | variable assign?
    | constant
    ;

splitop
    : SPLIT
    | TOKENS
    ;

constant
    : IntegerConstant
    | FloatingConstant
    | CharacterConstant
    | StringLit
    ;

print_
    : XPRINT
    | PRINTF
    | QUERY
    | SPRINTF
    ;

scan
    : SCANF
    | SSCANF
    ;

variable
    : ID index? members?
    ;

array_
    : ID
    ;

array
    : /* empty */
    | '[' ']'
    | '[' declare ']'
    ;

index
    : '[' expr ']'
    ;

args
    : /* empty */
    | arg_list
    ;

arg_list
    : expr      // %prec ','
    | arg_list ',' expr
    ;

formals
    : /* empty */
    | declare
    | formal_list
    ;

formal_list
    : formal_item
    | formal_list ',' formal_item
    ;

formal_item
    : declare name
    ;

members
    : member
    | '.' ID member
    ;

member
    : '.' ID
    ;

assign
    : AEQ expr
    | APEQ expr
    | AMEQ expr
    | ASEQ expr
    | ASLEQ expr
    ;

initialize_
    : assign
    ;

finitialize_
    : '(' formals ')' '{' statement_list? '}'
    ;

label : ID;
declare : (CHAR | DOUBLE | FLOAT | INT | LONG | UNSIGNED | VOID | STRING | ID) '*'? ;


AEQ : '=' ;
AMEQ : '-=' ;
AND : '&&' ;
APEQ : '+=' ;
ASEQ : '*=' ;
ASLEQ : '/=' ;
BREAK : 'break' ;
CASE : 'case' ;
CHAR : 'char' ;
CONTINUE : 'continue' ;
DEC : '--' ;
DEFAULT : 'default' ;
DOUBLE : 'double' ;
ELSE : 'else' ;
EQ : '==' ;
EXIT : 'exit' ;
FLOAT : 'float' ;
FOR : 'for' ;
function : ID ;
GE : '>=' ;
GSUB : 'gsub' ;
IF : 'if' ;
IN_OP : 'in' ;
INC : '++' ;
INT : 'int' ;
ITERATER : 'iterater' ;
LE : '<=' ;
LONG : 'long' ;
LSH : 'lsh' ;
NE : '!=' ;
OR : '||' ;
PRINTF : 'printf' ;
PROCEDURE : 'procedure' ;
QUERY : 'query' ;
RAND : 'rand' ;
RETURN : 'return' ;
RSH : 'rsh' ;
SCANF : 'scanf' ;
SPLIT : 'split' ;
SPRINTF : 'sprintf' ;
SRAND : 'srand' ;
SSCANF : 'sscanf' ;
STATIC : 'static' ;
STRING : 'string' ;
SUB : 'sub' ;
SUBSTR : 'substr' ;
SWITCH : 'switch' ;
TOKENS : 'tokens' ;
UNSET : 'unset' ;
UNSIGNED : 'unsigned' ;
VOID : 'void' ;
WHILE : 'while' ;
XPRINT : 'print' ;

StringLit
    :   EncodingPrefix? '"' SCharSequence? '"'
    ;

ID
    :   IdentifierNondigit
        (   IdentifierNondigit
        |   Digit
        )*
    ;

MLCOMMENT : '/*' .*? '*/' -> channel(HIDDEN);
SLCOMMENT : '//' ~[\n\r]* -> channel(HIDDEN);

WS : [ \t\n\r]+ -> channel(HIDDEN);

fragment
EncodingPrefix
    :   'u8'
    |   'u'
    |   'U'
    |   'L'
    ;

fragment
SCharSequence
    :   SChar+
    ;

fragment
SChar
    :   ~["\\\r\n]
    |   EscapeSequence
    |   '\\\n'   // Added line
    |   '\\\r\n' // Added line
    ;

fragment
IdentifierNondigit
    :   Nondigit
    |   UniversalCharacterName
    //|   // other implementation-defined characters...
    | '$'
    | '#'
    ;

fragment
Nondigit
    :   [a-zA-Z_]
    ;

fragment
Digit
    :   [0-9]
    ;

fragment
BinaryConstant
    : '0' [bB] [0-1]+
    ;

fragment
DecimalConstant
    :   NonzeroDigit Digit*
    ;

fragment
OctalConstant
    :   '0' OctalDigit*
    ;

fragment
HexadecimalConstant
    :   HexadecimalPrefix HexadecimalDigit+
    ;

fragment
HexadecimalPrefix
    :   '0' [xX]
    ;

fragment
NonzeroDigit
    :   [1-9]
    ;

fragment
OctalDigit
    :   [0-7]
    ;

fragment
HexadecimalDigit
    :   [0-9a-fA-F]
    ;

fragment
IntegerSuffix
    :   UnsignedSuffix LongSuffix?
    |   UnsignedSuffix LongLongSuffix
    |   LongSuffix UnsignedSuffix?
    |   LongLongSuffix UnsignedSuffix?
    ;

fragment
UnsignedSuffix
    :   [uU]
    ;

fragment
LongSuffix
    :   [lL]
    ;

fragment
LongLongSuffix
    :   'll' | 'LL'
    ;

fragment
DecimalFloatingConstant
    :   FractionalConstant ExponentPart? FloatingSuffix?
    |   DigitSequence ExponentPart FloatingSuffix?
    ;

fragment
HexadecimalFloatingConstant
    :   HexadecimalPrefix (HexadecimalFractionalConstant | HexadecimalDigitSequence) BinaryExponentPart FloatingSuffix?
    ;

fragment
FractionalConstant
    :   DigitSequence? '.' DigitSequence
    |   DigitSequence '.'
    ;

fragment
ExponentPart
    :   [eE] Sign? DigitSequence
    ;

fragment
Sign
    :   [+-]
    ;

fragment
UniversalCharacterName
    :   '\\u' HexQuad
    |   '\\U' HexQuad HexQuad
    ;

fragment
CCharSequence
    :   CChar+
    ;

fragment
CChar
    :   ~['\\\r\n]
    |   EscapeSequence
    ;


fragment
FloatingSuffix
    :   [flFL]
    ;

fragment
HexadecimalFractionalConstant
    :   HexadecimalDigitSequence? '.' HexadecimalDigitSequence
    |   HexadecimalDigitSequence '.'
    ;

fragment
HexadecimalDigitSequence
    :   HexadecimalDigit+
    ;

fragment
BinaryExponentPart
    :   [pP] Sign? DigitSequence
    ;

fragment
HexQuad
    :   HexadecimalDigit HexadecimalDigit HexadecimalDigit HexadecimalDigit
    ;

fragment
EscapeSequence
    :   SimpleEscapeSequence
    |   OctalEscapeSequence
    |   HexadecimalEscapeSequence
    |   UniversalCharacterName
    ;

fragment
SimpleEscapeSequence
    :   '\\' ['"?abfnrtv\\]
    ;

fragment
OctalEscapeSequence
    :   '\\' OctalDigit OctalDigit? OctalDigit?
    ;

fragment
HexadecimalEscapeSequence
    :   '\\x' HexadecimalDigit+
    ;

