/*
BSD License
Copyright (c) 2020, Tom Everett
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

/*
http://java.lykkenborg.no/2012/02/bcpl-grammar.html
*/
grammar bcpl;

element
   : CHARACTERCONSTANT
   | STRINGCONSTANT
   | NUMBER
   | IDENTIFIER
   | 'TRUE'
   | 'FALSE'
   ;

primaryE
   : primaryE '(' expression_list ')'
   | primaryE '(' ')'
   | '(' expression ')'
   | element
   ;

vectorE
   : (vectorE '!' primaryE)
   | primaryE
   ;

addressE
   : ADDRESSOP addressE
   | vectorE
   ;

multE
   : (multE MULTOP addressE)
   | addressE
   ;

addE
   : addE ADDOP multE
   | ADDOP multE
   | multE
   ;

relE
   : addE (RELOP addE)*
   ;

shiftE
   : shiftE SHIFTOP addE
   | relE
   ;

notE
   : NOTOP shiftE
   | shiftE
   ;

andE
   : notE (ADDOP notE)*
   ;

orE
   : andE (OROP andE)*
   ;

eqvE
   : orE (EQVOP orE)*
   ;
   // ugh

name
   : IDENTIFIER
   ;

conditionalE
   : eqvE '->' conditionalE ',' conditionalE
   | eqvE
   ;

expression
   : conditionalE
   | 'TABLE' constant_expression (',' constant_expression)*
   | 'VALOF' command
   ;

c_element
   : CHARACTERCONSTANT
   | NUMBER
   | IDENTIFIER
   | 'TRUE'
   | 'FALSE'
   | '(' constant_expression ')'
   ;

c_multE
   : c_multE MULTOP c_element
   | c_element
   ;

c_addE
   : c_addE ADDOP c_multE
   | ADDOP c_multE
   | c_multE
   ;

c_shiftE
   : c_shiftE SHIFTOP c_addE
   | c_addE
   ;

c_andE
   : c_andE ANDOP c_shiftE
   | c_shiftE
   ;

constant_expression
   : constant_expression OROP c_andE
   | c_andE
   ;

expression_list
   : expression (',' expression)*
   ;

name_list
   : name (',' name)*
   ;

manifest_item
   : IDENTIFIER '=' constant_expression
   ;

manifest_list
   : manifest_item (';' manifest_item)*
   ;

manifest_declaration
   : 'MANIFEST' '$(' manifest_list '$)'
   ;

static_declaration
   : 'STATIC' '$(' manifest_list '$)'
   ;

global_item
   : IDENTIFIER ':' constant_expression
   ;

global_list
   : global_item (';' global_item)*
   ;

global_declaration
   : 'GLOBAL' '$(' global_list '$)'
   ;

simple_definition
   : name_list '=' expression_list
   ;

vector_definition
   : IDENTIFIER '=' 'VEC' constant_expression
   ;

function_definition
   : IDENTIFIER '(' name_list ')' '=' expression
   | IDENTIFIER '(' ')' '=' expression
   ;

routine_definition
   : IDENTIFIER '(' name_list ')' 'BE' command
   | IDENTIFIER '(' ')' 'BE' command
   ;

definition
   : simple_definition
   | vector_definition
   | function_definition
   | routine_definition
   ;

simultaneous_declaration
   : 'LET' definition ('AND' definition)*
   ;

declaration
   : simultaneous_declaration
   | manifest_declaration
   | static_declaration
   | global_declaration
   ;

lhse
   : IDENTIFIER
   | vectorE '!' primaryE
   | '!' primaryE
   ;

left_hand_side_list
   : lhse (',' lhse)*
   ;

assignment
   : left_hand_side_list ':=' expression_list
   ;

simple_command
   : 'BREAK'
   | 'LOOP'
   | 'ENDCASE'
   | 'RETURN'
   | 'FINISH'
   ;

goto_command
   : 'GOTO' expression
   ;

routine_command
   : primaryE '(' expression_list ')'
   | primaryE '(' ')'
   ;

resultis_command
   : 'RESULTIS' expression
   ;

switchon_command
   : 'SWITCHON' expression 'INTO' compound_command
   ;

repeatable_command
   : assignment
   | simple_command
   | goto_command
   | routine_command
   | resultis_command
   | repeated_command
   | switchon_command
   | compound_command
   | block
   ;

repeated_command
   : repeatable_command 'REPEAT'
   | repeatable_command 'REPEATUNTIL' expression
   | repeatable_command 'REPEATWHILE' expression
   ;

until_command
   : 'UNTIL' expression 'DO' command
   ;

while_command
   : 'WHILE' expression 'DO' command
   ;

for_command
   : 'FOR' IDENTIFIER '=' expression 'TO' expression 'BY' constant_expression 'DO' command
   | 'FOR' IDENTIFIER ':' expression 'TO' expression 'DO' command
   ;

repetitive_command
   : repeated_command
   | until_command
   | while_command
   | for_command
   ;

test_command
   : 'TEST' expression 'THEN' command 'ELSE' command
   ;

if_command
   : 'IF' expression 'THEN' command
   ;

unless_command
   : 'UNLESS' expression 'THEN' command
   ;

unlabelled_command
   : repeatable_command
   | repetitive_command
   | test_command
   | if_command
   ;

label_prefix
   : IDENTIFIER ':'
   ;

case_prefix
   : 'CASE' constant_expression ':'
   ;

default_prefix
   : 'DEFAULT' ':'
   ;

prefix
   : label_prefix
   | case_prefix
   | default_prefix
   ;

command
   : unlabelled_command
   | prefix command
   | prefix
   ;

command_list
   : command (';' command)*
   ;

declaration_part
   : declaration (';' declaration)*
   ;

block
   : '$(' declaration_part ';' command_list '$)'
   ;

compound_command
   : '$(' command_list '$)'
   ;

program
   : declaration_part
   ;

LETTER
   : 'A'
   | 'B'
   | 'C'
   | 'D'
   | 'E'
   | 'F'
   | 'G'
   | 'H'
   | 'I'
   | 'J'
   | 'K'
   | 'L'
   | 'M'
   | 'N'
   | 'O'
   | 'P'
   | 'Q'
   | 'R'
   | 'S'
   | 'T'
   | 'U'
   | 'V'
   | 'W'
   | 'X'
   | 'Y'
   | 'Z'
   ;

OCTALDIGIT
   : '0'
   | '1'
   | '2'
   | '3'
   | '4'
   | '5'
   | '6'
   | '7'
   ;

HEXDIGIT
   : '0'
   | '1'
   | '2'
   | '3'
   | '4'
   | '5'
   | '6'
   | '7'
   | '8'
   | '9'
   | 'A'
   | 'B'
   | 'C'
   | 'D'
   | 'E'
   | 'F'
   ;

DIGIT
   : '0'
   | '1'
   | '2'
   | '3'
   | '4'
   | '5'
   | '6'
   | '7'
   | '8'
   | '9'
   ;

STRINGCONSTANT
   : '"' ('\'"' | ~ '"')* '"'
   ;

CHARACTERCONSTANT
   : '\'' (DIGIT | LETTER) '\''
   ;

OCTALNUMBER
   : '#' OCTALDIGIT+
   ;

HEXNUMBER
   : '#X' HEXDIGIT+
   ;

NUMBER
   : OCTALNUMBER
   | HEXNUMBER
   | DIGIT+
   ;

IDENTIFIER
   : LETTER (LETTER | DIGIT | '.')*
   ;

ADDRESSOP
   : '@'
   | '!'
   ;

MULTOP
   : '*'
   | '/'
   | 'REM'
   ;

ADDOP
   : '+'
   | '-'
   ;

RELOP
   : '='
   | '¬='
   | '<='
   | '>='
   | '<'
   | '>'
   ;

SHIFTOP
   : '<<'
   | '>>'
   ;

ANDOP
   : '&'
   ;

OROP
   : '|'
   ;

EQVOP
   : 'EQV'
   | 'NEQV'
   ;

NOTOP
   : '¬'
   ;

STRING
   :
   ;

WS
   : [ \r\n] -> skip
   ;

