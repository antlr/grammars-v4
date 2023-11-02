grammar gvpr;

preds: pred* EOF;

pred
    : 'BEGIN' ('{' program '}')?
    | 'BEG_G' ('{' program '}')?
    | 'N' ('[' expr ']')? ('{' program '}')?
    | 'E' ('[' expr ']')? ('{' program '}')?
    | 'END_G' ('{' program '}')?
    | 'END' ('{' program '}')?
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
    : statement_list action_list
    ;

action_list
    : /* empty */
    | action_list action_
    ;

action_
    : label ':' statement_list
    ;

statement_list
    : statement*
    ;

statement
    : '{' statement_list '}'
    | expr ';'?
    | static declare dcl_list ';'?
    | IF '(' expr ')' statement else_opt
    | FOR '(' variable ')' statement
    | FOR '(' expr_opt ';' expr_opt ';' expr_opt ')' statement
    | ITERATER '(' variable ')' statement
    | UNSET '(' dynamic ')'
    | UNSET '(' dynamic ',' expr  ')'
    | WHILE '(' expr ')' statement
    | SWITCH '(' expr ')' '{' switch_list '}'
    | BREAK expr_opt ';'?
    | CONTINUE expr_opt ';'?
    | RETURN expr_opt ';'?
    ;

switch_list
    : /* empty */
    | switch_list switch_item
    ;

switch_item
    : case_list statement_list
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
    : /* empty */
    | STATIC
    ;

dcl_list
    : dcl_item
    | dcl_list ',' dcl_item
    ;

dcl_item
    : dcl_name array initialize_
    ;

dcl_name
    : ID
    ;

name
    : ID
    ;

else_opt
    : /* empty */
    | ELSE statement
    ;

expr_opt
    : /* empty */
    | expr
    ;

expr
    : '(' expr ')'
    | '(' declare ')' expr // %prec CAST
    | expr ('*'|'/'|'%') expr
    | expr '<' expr
    | expr LSH expr
    | expr RSH expr
    | expr '>' expr
    | expr LE expr
    | expr GE expr
    | expr EQ expr
    | expr NE expr
    | expr '&' expr
    | expr '|' expr
    | expr '^' expr
    | expr '+' expr
    | expr AND expr
    | expr OR expr
    | expr ',' expr
    | expr '?' expr ':' expr
    | '!' expr
    | '#' dynamic
    | '~' expr
    | '-' expr  // %prec UNARY
    | '+' expr  // %prec UNARY
    | '&' variable  // %prec UNARY
    | array_ '[' args ']'
    | function '(' args ')'
    | GSUB '(' args ')'
    | SUB '(' args ')'
    | SUBSTR '(' args ')'
    | splitop '(' expr ',' dynamic ')'
    | splitop '(' expr ',' dynamic ',' expr ')'
    | EXIT '(' expr ')'
    | RAND '(' ')'
    | SRAND '(' ')'
    | SRAND '(' expr ')'
    | PROCEDURE '(' args ')'
    | print_ '(' args ')'
    | scan '(' args ')'
    | variable assign
    | INC variable
    | variable INC
    | expr IN_OP dynamic
    | DEC variable
    | variable DEC
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
    | STRING
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
    : dynamic index? members?
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
    : /* empty */
    | AEQ expr
    | APEQ expr
    | AMEQ expr
    | ASEQ expr
    | ASLEQ expr
    // '=' expr
    ;

initialize_
    : assign
    | '(' formals ')' '{' statement_list '}'
    ;

label : ID;


AND : '&&' ;
BREAK : 'break' ;
CASE : 'case' ;
CONTINUE : 'continue' ;
DEC : '--' ;
declare : ID;
DEFAULT : 'default' ;
dynamic : ID;
ELSE : 'else' ;
EQ : '==' ;
EXIT : 'exit' ;
FOR : 'for' ;
function : ID ;
GE : '>=' ;
GSUB : 'gsub' ;
IF : 'if' ;
IN_OP : 'in' ;
INC : '++' ;
ITERATER : 'iterater' ;
LE : '<=' ;
LSH : 'lsh' ;
NE : '!=' ;
OR : '||' ;
XPRINT : 'print' ;
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
SUB : 'sub' ;
SUBSTR : 'substr' ;
SWITCH : 'switch' ;
TOKENS : 'tokens' ;
UNSET : 'unset' ;
WHILE : 'while' ;
AEQ : '=' ;
APEQ : '+=' ;
AMEQ : '-=' ;
ASEQ : '*=' ;
ASLEQ : '/=' ;

STRING
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

