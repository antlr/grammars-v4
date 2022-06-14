/*
BSD License

Copyright (c) 2022, Tom Everett
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

grammar small;

prog : equation* EOF;

program::=#(statement | comment | command | separator). See Examples below.
outcome::= termination | nontermination.outcome::= termination | nontermination.
termination::=success | failure.
comment::= "//" #(char~end_of_line).
separator::= end_of_line whitespace.separator::= end_of_line whitespace.
whitespace::= #(end_of_line | ws)
ws::= #( horizontal_tab | space).
command::="edit" program_name | "exit"
statement::= structured_statement | expression_statement.
expression_statement::= expression O(assignment) O(disposition ).
assignment::= arrow variable.
disposition::= ignored | output | returned | testeddisposition::= ignored | output | returned | tested
arrow::="->"
expression::= O(prefix) ws simple_expression ws #(operator ws O(prefix) ws simple_expression ws).
E::expression->constant, E(e) is the value of expression e.
V::simple_expression->constant,
prefix::= "+" | "-" | "~".
operator::= arithmetic | relational | boolean | string_op.
arithmetic::="+" | "-" | "*" | "/" | "%".
relational::= "<" | "<=" | "<>" | "=" | ">=" | ">". "<", "<=", ">" and ">="
boolean::="&" | "|". "&" and "|" a
string_op::= ":" | default. The colon(":")
default::=""
simple_expression::= constant | variable | input | program_call.
input::="$".
program_call::= name_of_program.
variable::=letter.
constant::=string | integer | float.
string::= double_quotes #(char ~ double_quotes) double_quotes.
double_quotes::= "\"".
integer::= decimal_digit #(decimal_digit).
decimal_digit::= "0".."9".
float::= integer "." integer.
structured_statement::=iteration | selection
iteration::= "{" alternatives "}".
selection::= "[" alternatives "]".

alternatives::= alternative #( or_else alternative) .
or_else::= "||"
alternative::= program, -- as defined above.




WS
   : [ \r\n\t] + -> skip
   ;
