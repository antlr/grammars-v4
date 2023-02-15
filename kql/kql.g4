/*
BSD License

Copyright (c) 2023, Tom Everett
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

kql_expression : (operator_expression | expression_list) EOF;


expression_list : (operator_expression operator_expression)
 | (expression_list operator_expression);


operator_expression : (all | and | any | near | none | not | onear
 | or | words | xrank | basic_expression | paren_expression);

paren_expression : '(' kql_expression ')';

basic_expression : (QUALIFICATION? unQUOTED_STRING_VALUE)
 | (QUALIFICATION? QUOTED_STRING_VALUE)
 | property_restriction;


// Operator expressions
all : 'ALL' '(' string_value+ ')';

and : operator_expression 'AND' operator_expression;

any : 'ANY' '(' string_value+ ')';

none : 'NONE' '(' string_value+ ')';

not : 'NOT' operator_expression;

or : operator_expression 'OR' operator_expression;

near : operator_expression 'NEAR' proximity_param? operator_expression;

onear : operator_expression 'ONEAR' proximity_param? operator_expression;

proximity_param : '(' [['N' '='] integer_value] ')';

words : 'WORDS' '(' words_param_list ')';

words_param_list : words_param *([','] words_param);

words_param : QUALIFICATION? string_value;


xrank : operator_expression 'XRANK' '(' xrank_param_list ')' operator_expression;

xrank_param_list : xrank_param *([','] xrank_param);

xrank_param : ('pb' '=' float_value)
 | ('rb' '=' float_value)
 | ('cb' '=' float_value)
 | ('avgb' '=' float_value)
 | ('stdb' '=' float_value)
 | ('nb' '=' float_value)
 | ('n' '=' integer_value);

// Property restriction
property_restriction : QUALIFICATION?
 property_name PROPERTY_OPERATOR property_value;

property_name : property_token | QUOTED_STRING_VALUE;

property_token : 1*(%x30-39 | %x41-5a | %x5f | %x61-7a | %xaa | %xb5 | %xba
 | %xc0-d6 | %xe0-ffffffff);

property_value : property_typed_value
 | unquoted_property_token
 | QUOTED_STRING_VALUE;

PROPERTY_OPERATOR : ':' | '=' | '<>' | '>' | '>='| '<' | '<=';

unquoted_property_token : 1*(%x01-08 | %x0b-0c | %x0e-1f | %x21 | %x23-27
 | %x2a-3b | %x3d | %x3f-ffffffff);

property_typed_value : BOOLEAN_VALUE | %x22 BOOLEAN_VALUE %x22
 | float_value ['..' float_value]
 | %x22 float_value ['..' float_value] %x22
 | integer_value ['..' integer_value]
 | %x22 integer_value ['..' integer_value] %x22
 | date_named
 | date_value_no_ws ['..' date-value-no-ws]
 | %x22 date_value ['..' date-value] %x22;

date_named : 'today' | %x22 'today' %x22
 | 'yesterday' | %x22 'yesterday' %x22
 | %x22 'this week' %x22
 | %x22 'this month' %x22
 | %x22 'last month' %x22
 | %x22 'this year' %x22
 | %x22 'last year' %x22;


 BOOLEAN_VALUE : 'true' | 'false';

// The following are culture dependent and are not specified here:
// float_value, integer_value, date-value, date-value-no-ws
string_value : QUOTED_STRING_VALUE | UNQUOTED_STRING_VALUE;

// <QUOTED_STRING_VALUE> can contain any characters, but a double quotation
// mark within the quoted string MUST be represented by two double quotation marks.

QUOTED_STRING_VALUE : DQUOTE 1*(%x00-21 | DQUOTE DQUOTE | %x23-ffffffff) DQUOTE;
// <unQUOTED_STRING_VALUE> cannot contain white space,
// double quotation mark, and parentheses.
// <unQUOTED_STRING_VALUE> can contain property-chars in the beginning or at
// the end, but not in the middle
UNQUOTED_STRING_VALUE : *PROPERTY_CHARS
 *(%x01-08 | %x0b-0c | %x0e-1f | %x21 | %x23-27 | %x2a-39 | %x3b
 | %x3f-ffffffff)
 *PROPERTY_CHARS;

PROPERTY_CHARS : ':' | '=' | '>' | '<';

// General syntax element
QUALIFICATION : '+' | '-';

WS
   : [ \r\n\t] + -> skip
   ;
