parser grammar ElixirParser;

options {
  tokenVocab=ElixirLexer;
}

parse
 : block EOF
 ;

block
 : eoe? expression ( eoe expression )* eoe?
 ;

eoe
 : ( ';' | NL )+
 ;

expression
 : expression expression_tail                                        #dotExpr
 | expression '.'? '(' ( expressions_ ( ',' NL* options_ )? )? ')'   #functionCallExpr
 | expression '.'? '(' NL* options_ NL*')'                           #functionCallExpr
 | module_def                                                        #moduleDefExpr
 | function_def                                                      #functionDefExpr
 | macro_def                                                         #macroDefExpr
 | for                                                               #forExpr
 | with                                                              #withExpr
 | try                                                               #tryExpr
 | expression expressions_ ( ',' NL* options_ )?                     #functionCallExpr
 | expression options_? expressions_                                 #functionCallExpr
 | expression options_                                               #functionCallExpr
 | anonymous_function                                                #anonymousFunctionExpr
 | '(' expression ')'                                                #nestedExpr
 | '@' expression options_?                                          #atExpr
 | unaryOp expression                                                #unaryExpr
 | expression mulOp expression                                       #mulExpr
 | expression addOp expression                                       #addExpr
 | expression listOp expression                                      #listExpr
 | expression inOp expression                                        #inExpr
 | expression '|>' expression                                        #pipeExpr
 | expression otherOp expression                                     #otherExpr
 | expression bitOp expression                                       #bitExpr
 | expression relOp expression                                       #relExpr
 | expression eqOp expression                                        #eqExpr
 | expression andOp expression                                       #andExpr
 | expression orOp expression                                        #orExpr
 | expression '=' expression                                         #patternExpr
 | '&' expression                                                    #ampExpr
 | expression '|' expression                                         #prependExpr
 | expression '::' expression                                        #typeExpr
 | expression WHEN expression                                        #whenExpr
 | expression '\\\\' expression                                      #defaultValueExpr
 | expression '->' NL* expression                                    #rarrowExpr
 | expression '<-' expression                                        #larrowExpr
 | list                                                              #listExpr
 | tuple                                                             #tupleExpr
 | map                                                               #mapExpr
 | bool_                                                             #boolExpr
 | bitstring                                                         #bitstringExpr
 | case                                                              #caseExpr
 | cond                                                              #condExpr
 | if                                                                #ifExpr
 | unless                                                            #unlessExpr
 | operator                                                          #operatorExpr
 | do_block                                                          #doBlockExpr
 | variables                                                         #variablesExpr
 | ATOM                                                              #atomExpr
 | INTEGER                                                           #integerExpr
 | HEXADECIMAL                                                       #hexadecimalExpr
 | OCTAL                                                             #octalExpr
 | BINARY                                                            #binaryExpr
 | FLOAT                                                             #floatExpr
 | SIGIL                                                             #sigilExpr
 | SINGLE_LINE_STRING                                                #singleLineStringExpr
 | MULTI_LINE_STRING                                                 #multiLineStringExpr
 | SINGLE_LINE_CHARLIST                                              #singleLineCharlistExpr
 | MULTI_LINE_CHARLIST                                               #multiLineCharlistExpr
 | ALIAS                                                             #aliasExpr
 | CODEPOINT                                                         #codepointExpr
 | NIL                                                               #nilExpr
 ;

unaryOp : '+' | '-' | '!' | '^' | 'not' | '~~~';
mulOp : '*' | '/';
addOp : '+' | '-';
listOp : '++' | '--' | '+++' | '---' | '..' | '<>';
inOp :  'in' | 'not' 'in';
relOp : '<' | '>' | '<=' | '>=';
eqOp : '==' | '!=' | '=~' | '===' | '!==';
andOp : '&&' | '&&&' | 'and';
orOp : '||' | '|||' | 'or';
bitOp : '<<<' | '>>>';
otherOp : '<<~' | '~>>' | '<~' | '~>' | '<~>' | '<|>';

// TODO add literals
operator : unaryOp | mulOp | addOp | listOp | inOp | relOp | eqOp | andOp | orOp | bitOp | otherOp;

expression_tail
 : '[' expression ']'
 | '.' expression
 ;

do_block
 : DO NL* block? NL* ( AFTER NL* block NL* )? END
 ;

bool_
 : TRUE
 | FALSE
 ;

list
 : '[' NL* expressions_ ','? NL* ']'
 | '[' NL* ( expressions_ ','? NL* )? ']'
 | '[' NL* tuples ( ',' short_map_entries )? NL* ']'
 | '[' NL* short_map_entries NL* ']'
 ;

tuples
 : tuple ( ',' tuple )*
 ;

tuple
 : '{' ( expressions_ ','? )? '}'
 ;

map
 : OMAP NL* '}'
 | OMAP NL* ( expression '|' )? map_entries ','? NL* '}'
 | OMAP NL* ( expression '|' )? short_map_entries ','? NL* '}'
 | OMAP NL* ( expression '|' )? map_entries ( ',' short_map_entries )? ','? NL* '}'
 ;

map_entries
 : map_entry ( ',' map_entry )*
 ;

map_entry
 : expression '=>' expression
 ;

short_map_entries
 : short_map_entry ( ',' NL* short_map_entry )*
 ;

short_map_entry
 : variable ':' expression
 ;

anonymous_function
 : FN NL* expressions_? '->' NL* block? NL* END
 | FN '(' expressions_? ')' '->' NL* block? NL* END
 ;

case
 : CASE expression DO NL+ condition+ END
 ;

condition
 : expression '->' NL* expression NL+
 ;

cond
 : COND DO NL+ condition+ END
 ;

if
 : IF expression DO NL* block NL* ( ELSE NL* block NL* )? END
 | IF expression ',' NL* DO ':' NL* expression ( ',' NL* ELSE ':' NL* expression )?
 ;

unless
 : UNLESS expression do_block
 ;

bitstring
 : '<<' ( expressions_ ','? )? '>>'
 ;

module_def
 : DEFMODULE ALIAS do_block
 ;

function_def
 : ( DEF | DEFP ) variable ( '(' expressions_? ')' )+ ( WHEN expression )? do_block
 | ( DEF | DEFP ) variable ( '(' expressions_? ')' )+ ( WHEN expression )? ',' DO ':' expression
 | ( DEF | DEFP ) variable ',' DO ':' expression
 ;

macro_def
 : DEFMACRO variable '(' expressions_? ')' ( WHEN expression )? do_block
 | DEFMACRO variable '(' expressions_? ')' ( WHEN expression )? ',' DO ':' expression
 ;

for
 : FOR expressions_ ( ',' NL* options_ )? do_block
 | FOR expressions_ ( ',' NL* options_ )? ',' DO ':' NL* expression
 ;

options_
 : option ( ',' NL* option )*
 ;

option
 : variable ':' expression
 ;

with
 : WITH expressions_ do_block
 ;

try
 : TRY DO NL* block ( RECUE | CATCH | AFTER ) NL* expressions_ ( NL* block )? ( NL* ELSE NL* block )? NL* END
 ;

expressions_
 : expression ( ',' NL* expression )*
 ;

variables
 : variable ( ',' NL* variable )*
 ;

variable
 : VARIABLE
 | CASE
 | COND
 | IF
 | UNLESS
 | DEFMODULE
 | DEFMACRO
 | DEF
 | DEFP
 | FOR
 | WITH
 | TRY
 ;