parser grammar  gvprParser;

options { tokenVocab = gvprLexer; superClass = GvprParserBase; }

// Insert here @header for C++ parser.

preds: pred* EOF;

pred
    : 'BEGIN' ('{' program '}')?
    | 'BEG_G' ('{' program '}')?
    | 'N' ('[' program ']')? ('{' program '}')?
    | 'E' ('[' program ']')? ('{' program '}')?
    | 'END_G' ('{' program '}')?
    | 'END' ('{' program '}')?
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
    : statement ( ( { this.IsSemiRequired() }? ';' | { this.IsSemiNotRequired() }? ) statement )* ';'?
    ;

statement
    : '{' statement_list? '}'
    | static? declare dcl_list
    | static? declare fdcl_item
    | expr
    | 'if' '(' expr ')' statement (';'? else_)?
    | 'for' '(' variable ')' statement
    | 'for' '(' expr? ';' expr? ';' expr? ')' statement
    | 'forr' '(' variable ')' statement
    | UNSET '(' ID ')'
    | UNSET '(' ID ',' expr  ')'
    | WHILE '(' expr ')' statement
    | SWITCH '(' expr ')' '{' switch_list '}'
    | BREAK expr?
    | CONTINUE expr?
    | RETURN expr?
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
    : dcl_name array? initialize_?
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
    : (ID | '$') index? members?
    ;

array_
    : ID
    ;

array
    : '[' ']'
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
    : ('.' (ID | SPLIT))+
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
function : ID ;
