/*
BSD License

Copyright (c) 2026, Tom Everett
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions
are met:

1. Redistributions of source code must retain the above copyright
   notice, this list of conditions and the following disclaimer.
2. Redistributions in binary form must reproduce the above copyright
   notice, this list of conditions and the following disclaimer in the
   documentation and/or other materials provided with the distribution.
3. Neither the name of Tom Everett nor the names of its contributors
   may be used to endorse or promote products derived from this software
   without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
"AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*/
grammar kql;

kql_expression
   : operator_expression EOF
   ;

operator_expression
   : operator_expression operator_expression                                 # implicitAndExpression
   | operator_expression 'AND' operator_expression                           # andExpression
   | operator_expression 'OR' operator_expression                            # orExpression
   | operator_expression 'NEAR' proximity_param? operator_expression         # nearExpression
   | operator_expression 'ONEAR' proximity_param? operator_expression        # onearExpression
   | operator_expression 'XRANK' '(' xrank_param_list ')' operator_expression # xrankExpression
   | 'NOT' operator_expression                                               # notExpression
   | 'ALL' '(' string_value+ ')'                                             # allExpression
   | 'ANY' '(' string_value+ ')'                                             # anyExpression
   | 'NONE' '(' string_value+ ')'                                            # noneExpression
   | 'WORDS' '(' words_param_list ')'                                        # wordsExpression
   | basic_expression                                                        # basicExpression
   | paren_expression                                                        # parenExpression
   ;

paren_expression
   : '(' kql_expression ')'
   ;

basic_expression
   : (QUALIFICATION? UNQUOTED_STRING_VALUE)
   | (QUALIFICATION? QUOTED_STRING_VALUE)
   | (QUALIFICATION? PROPERTY_TOKEN)
   | property_restriction
   ;
   // Operator expressions

proximity_param
   : '(' (('N' '=')? INTEGER_VALUE)? ')'
   ;

words_param_list
   : words_param ((',')? words_param)*
   ;

words_param
   : QUALIFICATION? string_value
   ;

string_value
   : QUOTED_STRING_VALUE
   | UNQUOTED_STRING_VALUE
   ;

xrank_param_list
   : xrank_param ((',')? xrank_param)*
   ;

xrank_param
   : ('pb' '=' FLOAT_VALUE)
   | ('rb' '=' FLOAT_VALUE)
   | ('cb' '=' FLOAT_VALUE)
   | ('avgb' '=' FLOAT_VALUE)
   | ('stdb' '=' FLOAT_VALUE)
   | ('nb' '=' FLOAT_VALUE)
   | ('n' '=' INTEGER_VALUE)
   ;
   // Property restriction

property_restriction
   : QUALIFICATION? property_name PROPERTY_OPERATOR property_value
   ;

property_name
   : PROPERTY_TOKEN
   | QUOTED_STRING_VALUE
   ;

INTEGER_VALUE
   : [0-9]+
   ;

FLOAT_VALUE
   : [0-9]+ '.' [0-9]*
   ;

PROPERTY_TOKEN
   : ('\u0030' .. '\u0039' | '\u0041' .. '\u005a' | '\u005f' | '\u0061' .. '\u007a' | '\u00aa' | '\u00b5' | '\u00ba' | '\u00c0' .. '\u00d6' | '\u00e0' .. '\uffff')+
   ;

property_value
   : property_typed_value
   | UNQUOTED_PROPERTY_TOKEN
   | PROPERTY_TOKEN
   | QUOTED_STRING_VALUE
   ;

UNQUOTED_PROPERTY_TOKEN
   : ('\u0001' .. '\u0008' | '\u000b' .. '\u000c' | '\u000e' .. '\u001f' | '\u0021' | '\u0023' .. '\u0027' | '\u002a' .. '\u0039' | '\u003b' | '\u003f' .. '\uffff')+
   ;

property_typed_value
   : boolean_value
   | '\u0022' boolean_value '\u0022'
   | FLOAT_VALUE ('..' FLOAT_VALUE)?
   | '\u0022' FLOAT_VALUE ('..' FLOAT_VALUE)? '\u0022'
   | INTEGER_VALUE ('..' INTEGER_VALUE)?
   | '\u0022' INTEGER_VALUE ('..' INTEGER_VALUE)? '\u0022'
   | date_named
   ;

boolean_value
   : 'true'
   | 'false'
   ;

date_named
   : 'today'
   | '\u0022' 'today' '\u0022'
   | 'yesterday'
   | '\u0022' 'yesterday' '\u0022'
   | '\u0022' 'this week' '\u0022'
   | '\u0022' 'this month' '\u0022'
   | '\u0022' 'last month' '\u0022'
   | '\u0022' 'this year' '\u0022'
   | '\u0022' 'last year' '\u0022'
   ;

PROPERTY_OPERATOR
   : ':'
   | '='
   | '<>'
   | '>'
   | '>='
   | '<'
   | '<='
   ;

   // The following are culture dependent and are not specified here:

   // float_value, integer_value, date-value, date-value-no-ws

   // <QUOTED_STRING_VALUE> can contain any characters, but a double quotation

   // mark within the quoted string MUST be represented by two double quotation marks.

QUOTED_STRING_VALUE
   : DQUOTE ('\u0000' .. '\u0021' | DQUOTE DQUOTE | '\u0023' .. '\uffff')+ DQUOTE
   ;
   // <unQUOTED_STRING_VALUE> cannot contain white space,

   // double quotation mark, and parentheses.

   // <unQUOTED_STRING_VALUE> can contain property-chars in the beginning or at

   // the end, but not in the middle

UNQUOTED_STRING_VALUE
   : ('\u0001' .. '\u0008' | '\u000b' .. '\u000c' | '\u000e' .. '\u001f' | '\u0021' | '\u0023' .. '\u0027' | '\u002a' .. '\u0039' | '\u003b' | '\u003f' .. '\uffff')+
   ;

DQUOTE
   : '"'
   ;
   // General syntax element

QUALIFICATION
   : '+'
   | '-'
   ;

WS
   : [ \r\n\t]+ -> skip
   ;

