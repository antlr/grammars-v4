lexer grammar VTLLexer;

tokens {
  OPAR, CPAR, OBRACK, CBRACK, OBRACE, CBRACE, STRING, INTEGER, ID, REFERENCE, DOT, COMMA, ASSIGN, EQ, NE, AND, OR,
  K_NULL, ADD, SUB, MUL, DIV, MOD, COLON, FLOAT, RANGE, LT, LE, GT, GE, EXCL, K_LT, K_LE, K_GT, K_GE, K_EQ, K_NE,
  K_TRUE, K_FALSE, K_AND, K_OR, K_NOT, K_NULL, K_IN, IF, ELSEIF, ELSE, FOREACH, SET, END, BREAK, MACRO_ID, MACRO,
  STOP, INCLUDE, EVALUATE, PARSE, DEFINE
}

ESCAPED_CHAR
 : '\\' .
 ;

START_DIRECTIVE
 : '#' -> skip, pushMode(DIR_)
 ;

DOLLAR_EXCL_OBRACE
 : '$' '\\'* '!{' -> pushMode(FRM_)
 ;

DOLLAR_OBRACE
 : '$' '{' -> pushMode(FRM_)
 ;

DOLLAR_EXCL
 : '$' '\\'* '!' -> pushMode(VAR_)
 ;

DOLLAR
 : '$' -> pushMode(VAR_)
 ;

TEXT
 : .
 ;

// Formal mode
mode FRM_;

FRM_ID
 : ID -> type(ID)
 ;

FRM_DOT
 : '.' -> type(DOT)
 ;

FRM_OBRACK
 : '[' -> type(OBRACK), pushMode(IDX_)
 ;

FRM_OPAR
 : '(' -> type(OPAR), pushMode(CODE_)
 ;

FRM_CBRACE
 : '}' -> type(CBRACE), popMode
 ;

// Directive mode
mode DIR_;

ESCAPED_BLOCK
 : '[[' .*? ']]#' -> popMode
 ;

SNGLE_LINE_COMMENT
 : '#' ~[\r\n]* -> skip , popMode
 ;

VTL_COMMENT_BLOCK
 : '**' .*? '*#' -> channel(HIDDEN), popMode
 ;

MULTI_LINE_COMMENT
 : '*' .*? '*#' -> skip, popMode
 ;

DIR_SET
 : ( 'set' | '{set}' ) SPACES? '(' -> type(SET), popMode, pushMode(CODE_)
 ;

DIR_FOREACH
 : ( 'foreach' | '{foreach}' ) SPACES? '(' -> type(FOREACH), popMode, pushMode(CODE_)
 ;

DIR_IF
 : ( 'if' | '{if}' ) SPACES? '(' -> type(IF), popMode, pushMode(CODE_)
 ;

DIR_ELSEIF
 : ( 'elseif' | '{elseif}' ) SPACES? '(' -> type(ELSEIF), popMode, pushMode(CODE_)
 ;

DIR_ELSE
 : ( 'else' | '{else}' ) -> type(ELSE), popMode
 ;

DIR_INCLUDE
 : ( 'include' | '{include}' ) SPACES? '(' -> type(INCLUDE), popMode, pushMode(CODE_)
 ;

DIR_PARSE
 : ( 'parse' | '{parse}' ) SPACES? '(' -> type(PARSE), popMode, pushMode(CODE_)
 ;

DIR_EVALUATE
 : ( 'evaluate' | '{evaluate}' ) SPACES? '(' -> type(EVALUATE), popMode, pushMode(CODE_)
 ;

DIR_DEFINE
 : ( 'define' | '{define}' ) SPACES? '(' -> type(DEFINE), popMode, pushMode(CODE_)
 ;

DIR_STOP
 : ( 'stop' | '{stop}' ) -> type(STOP), popMode
 ;

DIR_BREAK
 : ( 'break' | '{break}' ) -> type(BREAK), popMode
 ;

DIR_END
 : ( 'end' | '{end}' ) -> type(END), popMode
 ;

DIR_MACRO
 : ( 'macro' | '{macro}' ) SPACES? '(' -> type(MACRO), popMode, pushMode(CODE_)
 ;

DIR_MACRO_CALL
 : '@' ID SPACES? '(' -> type(MACRO_ID), popMode, pushMode(CODE_)
 ;

DIR_CUSTOM_CODE
 : ID SPACES? '(' -> type(ID), popMode, pushMode(CODE_)
 ;

DIR_CUSTOM
 : ID -> type(ID), popMode
 ;

// Variable mode
mode VAR_;

VAR_DOLLAR
 : '$'  -> type(DOLLAR)
 ;

VAR_DOLLAR_EXCL
 : '$' '\\'* '!' -> type(DOLLAR_EXCL)
 ;

VAR_DOLLAR_EXCL_OBRACE
 : '$' '\\'* '!{' -> type(DOLLAR_EXCL_OBRACE), popMode, pushMode(FRM_)
 ;

VAR_DOLLAR_OBRACE
 : '$' '{' -> type(DOLLAR_OBRACE), popMode, pushMode(FRM_)
 ;

VAR_HASH
 : '#' -> skip, popMode, pushMode(DIR_)
 ;

VAR_ID
 : ID -> type(ID)
 ;

VAR_DOT
 : '.' -> type(DOT)
 ;

VAR_OBRACK
 : '[' -> type(OBRACK), pushMode(IDX_)
 ;

VAR_OPAR
 : '(' -> type(OPAR), pushMode(CODE_)
 ;

VAR_TEXT
 : . -> type(TEXT), popMode
 ;

// Code mode
mode CODE_;

CODE_K_LT
 : 'lt' -> type(K_LT)
 ;

CODE_K_LE
 : 'le' -> type(K_LE)
 ;

CODE_K_GT
 : 'gt' -> type(K_GT)
 ;

CODE_K_GE
 : 'ge' -> type(K_GE)
 ;

CODE_K_EQ
 : 'eq' -> type(K_EQ)
 ;

CODE_K_NE
 : 'ne' -> type(K_NE)
 ;

CODE_K_TRUE
 : 'true' -> type(K_TRUE)
 ;

CODE_K_FALSE
 : 'false' -> type(K_FALSE)
 ;

CODE_K_AND
 : 'and' -> type(K_AND)
 ;

CODE_K_OR
 : 'or' -> type(K_OR)
 ;

CODE_K_NOT
 : 'not' -> type(K_NOT)
 ;

CODE_K_NULL
 : 'null' -> type(K_NULL)
 ;

CODE_K_IN
 : 'in' -> type(K_IN)
 ;

CODE_ID
 : ID -> type(ID)
 ;

CODE_ADD
 : '+' -> type(ADD)
 ;

CODE_SUB
 : '-' -> type(SUB)
 ;

CODE_MUL
 : '*' -> type(MUL)
 ;

CODE_DIV
 : '/' -> type(DIV)
 ;

CODE_MOD
 : '%' -> type(MOD)
 ;

CODE_EXCL
 : '!' -> type(EXCL)
 ;

CODE_OR
 : '||' -> type(OR)
 ;

CODE_AND
 : '&&' -> type(AND)
 ;

CODE_ASSIGN
 : '=' -> type(ASSIGN)
 ;

CODE_EQ
 : '==' -> type(EQ)
 ;

CODE_NEQ
 : '!=' -> type(NE)
 ;

CODE_LT
 : '<' -> type(LT)
 ;

CODE_LE
 : '<=' -> type(LE)
 ;

CODE_GT
 : '>' -> type(GT)
 ;

CODE_GE
 : '>=' -> type(GE)
 ;

CODE_SPACES
 : SPACES -> skip
 ;

CODE_REFERENCE
 : '$' ID -> type(REFERENCE)
 ;

CODE_OPAR
 : '(' -> type(OPAR), pushMode(CODE_)
 ;

CODE_CPAR
 : ')' -> type(CPAR), popMode
 ;

CODE_COLON
 : ':' -> type(COLON)
 ;

CODE_RANGE
 : '..' -> type(RANGE)
 ;

CODE_DOT
 : '.' -> type(DOT)
 ;

CODE_FLOAT
 : FLOAT -> type(FLOAT)
 ;

CODE_INTEGER
 : INTEGER -> type(INTEGER)
 ;

CODE_STRING
 : STRING -> type(STRING)
 ;

CODE_OBRACK
 : '[' -> type(OBRACK)
 ;

CODE_CBRACK
 : ']' -> type(CBRACK)
 ;

CODE_OBRACE
 : '{' -> type(OBRACE)
 ;

CODE_CBRACE
 : '}' -> type(CBRACE)
 ;

CODE_COMMA
 : ',' -> type(COMMA)
 ;

// Index mode
mode IDX_;

IDX_CBRACK
 : ']' -> type(CBRACK), popMode
 ;

IDX_REFERENCE
 : '$' ID -> type(REFERENCE)
 ;

IDX_STRING
 : STRING -> type(STRING)
 ;

IDX_INTEGER
 : INTEGER -> type(INTEGER)
 ;

fragment STRING
 : STRING_DQ
 | STRING_SQ
 ;

fragment FLOAT
 : DIGIT* '.' DIGIT+ EXPONENT?
 | DIGIT+ '.' {this._input.LA(1) != '.'}? EXPONENT?
 | DIGIT+ EXPONENT
 ;

fragment SPACES    : [ \t\r\n];
fragment ID        : [a-zA-Z] [a-zA-Z0-9_-]*;
fragment STRING_DQ : '"' ( ~["\r\n] | '""' )* '"';
fragment STRING_SQ : '\'' ( ~['\r\n] | '\'\'' )* '\'';
fragment INTEGER   : DIGIT+;
fragment DIGIT     : [0-9];
fragment EXPONENT  : [eE] [+-]? DIGIT+;
