parser grammar VTLParser;

options {
  tokenVocab=VTLLexer;
}

parse
 : block EOF
 ;

block
 : atom*
 ;

atom
 : TEXT
 | ESCAPED_CHAR
 | ESCAPED_BLOCK
 | variable
 | formal
 | property_or_method
 | directive
 ;

formal
 : DOLLAR_OBRACE formal_property_or_method CBRACE
 | DOLLAR_EXCL_OBRACE formal_property_or_method CBRACE
 | DOLLAR_OBRACE id CBRACE
 | DOLLAR_EXCL_OBRACE id CBRACE
 ;

variable
 : DOLLAR id DOT?
 | DOLLAR_EXCL id DOT?
 | REFERENCE DOT?
 ;

property_or_method
 : variable property_end+
 ;

formal_property_or_method
 : id property_end+
 ;

directive
 : set_directive
 | if_directive
 | foreach_directive
 | break_directive
 | stop_directive
 | macro_directive
 | parse_directive
 | define_directive
 | include_directive
 | evaluate_directive
 | macro_call_directive
 | custom_directive
 ;

property_end
 : DOT ID
 | OBRACK expression CBRACK
 | OPAR expressions? CPAR
 ;

expressions
 : expression ( COMMA expression )*
 ;

set_directive
 : HASH SET expression ASSIGN expression CPAR
 ;

if_directive
 : HASH IF expression CPAR block elseif_directive* else_directive? end
 ;

elseif_directive
 : HASH ELSEIF expression CPAR block
 ;

else_directive
 : HASH ELSE block
 ;

foreach_directive
 : HASH FOREACH variable K_IN expression CPAR block end
 ;

break_directive
 : HASH BREAK
 ;

stop_directive
 : HASH STOP
 ;

custom_directive
 : HASH ID ( expression* CPAR )? ( block end )?
 ;

macro_directive
 : HASH MACRO expression* CPAR block end
 ;

parse_directive
 : HASH PARSE expression CPAR
 ;

define_directive
 : HASH DEFINE expression CPAR block end
 ;

include_directive
 : HASH INCLUDE expressions CPAR
 ;

evaluate_directive
 : HASH EVALUATE expression CPAR
 ;

macro_call_directive
 : HASH MACRO_ID expression* CPAR block end
 ;

end
 : HASH END
 ;

// Operator precedence is as v2.0 defined it:
// https://velocity.apache.org/engine/2.0/upgrading.html
expression
 : ( EXCL | K_NOT ) expression
 | SUB expression
 | expression ( MUL | DIV | MOD ) expression
 | expression ( ADD | SUB ) expression
 | expression ( EQ | NE | K_EQ | K_NE ) expression
 | expression ( LT | LE | GT | GE | K_LT | K_LE | K_GT | K_GE ) expression
 | expression ( AND | K_AND ) expression
 | expression ( OR | K_OR ) expression
 | expression RANGE expression
 | list
 | map
 | property_or_method
 | variable
 | id
 | STRING
 | INTEGER
 | FLOAT
 | K_NULL
 ;

list
 : OBRACK expressions? CBRACK
 ;

map
 : OBRACE map_entries? CBRACE
 ;

map_entries
 : map_entry ( COMMA map_entry )*
 ;

map_entry
 : expression COLON expression
 ;

id
 : ID
 | K_LT
 | K_LE
 | K_GT
 | K_GE
 | K_EQ
 | K_NE
 | K_TRUE
 | K_FALSE
 | K_AND
 | K_OR
 | K_NOT
 | K_NULL
 | K_IN
 ;
