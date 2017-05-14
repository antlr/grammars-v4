/*
BSD License

Copyright (c) 2013, Tom Everett
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

grammar mumps;

program
   : line + eof
   ;

eof
   : SPACE* CR?
   ;

line
   : code
   | routinedecl
   | CR
   ;

code
   : (label | (SPACE + DOT +) | SPACE +) (command + | if_ | subproc)? SPACE* CR
   ;

/*
* A line may begin with a label. If so, the label must begin in column one.
* After a label there must be at least one blank or a <tab> character before the
* first command.
* If there is no label, column one must be a blank or a <tab> character followed by
* some number of blanks, possibly zero, before the first command.
*/
label
   : identifier SPACE +
   ;

routinedecl
   : PERCENT? identifier (LPAREN paramlist? RPAREN)? SPACE* CR
   ;

paramlist
   : param (COMMA param)*
   ;

param
   : variable
   ;

subproc
   : identifier (LPAREN paramlist? RPAREN)? (SPACE* command) +
   ;

command
   : set_
   | for_
   | write_
   | read_
   | quit_
   | halt_
   | hang_
   | new_
   | break_
   | do_
   | kill_
   | view_
   | merge_
   | xecute_
   | (CLOSE | ELSE | GOTO | JOB | LOCK | OPEN | TCOMMIT | TRESTART | TROLLBACK | TSTART | USE)
   ;

postcondition
   : COLON expression
   ;

expression
   : term (SPACE* (ADD | MULTIPLY | SUBTRACT | DIVIDE | INTDIVIDE | MODULO | EXPONENT) expression)*
   ;

term
   : variable
   | NUMBER
   | LPAREN expression RPAREN
   ;

condition
   : term
   | (term (NGT | NLT | LT | GT | EQUALS) term)
   ;

identifier
   : IDENTIFIER
   ;

variable
   : (CARAT | DOLLAR | AMPERSAND)* identifier (LPAREN arglist RPAREN)?
   ;

/*
* COMMANDS
*
* After most command words or abbreviations there may be an optional postconditional.
* No blanks or <tab> characters are permitted between the command
* word and the post-conditional.
* If a command has an argument, there must be at least one blank after the
* command word and its post-conditional, if present, and the argument.
*/
break_
   : (BREAK) postcondition?
   ;

do_
   : (DO) postcondition? SPACE + identifier (LPAREN paramlist? RPAREN)?
   ;

for_
   : FOR SPACE + term EQUALS term COLON (term COLON)? term SPACE + (command SPACE?)* COLON SPACE* condition
   ;

halt_
   : (HALT) postcondition?
   ;

hang_
   : HANG postcondition? SPACE + term
   ;

if_
   : IF SPACE + condition SPACE* command
   ;

kill_
   : KILL postcondition? SPACE + arglist
   ;

merge_
   : MERGE postcondition? SPACE + term EQUALS term (',' term EQUALS term)*
   ;

new_
   : (NEW) postcondition? SPACE + arglist
   ;

quit_
   : (QUIT) postcondition? (SPACE + term)?
   ;

read_
   : (READ) postcondition? SPACE + arglist
   ;

set_
   : (SET) postcondition? SPACE + assign (',' assign)*
   ;

view_
   : VIEW postcondition? SPACE + IDENTIFIER
   ;

write_
   : (WRITE) postcondition? SPACE + arglist
   ;

xecute_
   : XECUTE postcondition? SPACE + STRING_LITERAL
   ;

assign
   : (LPAREN? arglist RPAREN?)? SPACE* EQUALS SPACE* arg
   ;

arglist
   : arg (SPACE* COMMA arg)*
   ;

arg
   : expression
   | (BANG | STRING_LITERAL)
   ;

/*
* Command Tokens
*/

BREAK
   : B R E A K
   ;


CLOSE
   : C L O S E
   ;


DO
   : D O
   ;


ELSE
   : E L S E
   ;


FOR
   : F O R
   ;


GOTO
   : G O T O
   ;


HALT
   : H A L T
   ;


HANG
   : H A N G
   ;


IF
   : I F
   ;


JOB
   : J O B
   ;


KILL
   : K I L L
   ;


LOCK
   : L O C K
   ;


MERGE
   : M E R G E
   ;


NEW
   : N E W
   ;


OPEN
   : O P E N
   ;


QUIT
   : Q U I T
   ;


READ
   : R E A D
   ;


SET
   : S E T
   ;


TCOMMIT
   : T C O M M I T
   ;


TRESTART
   : T R E S T A R T
   ;


TROLLBACK
   : T R O L L B A C K
   ;


TSTART
   : T S T A R T
   ;


USE
   : U S E
   ;


VIEW
   : V I E W
   ;


WRITE
   : W R I T E
   ;


XECUTE
   : X E C U T E
   ;


COLON
   : ':'
   ;


COMMA
   : ','
   ;


DOLLAR
   : '$'
   ;


PERCENT
   : '%'
   ;


AMPERSAND
   : '&'
   ;


INDIRECT
   : '@'
   ;


CARAT
   : '^'
   ;


BANG
   : '!'
   ;


LPAREN
   : '('
   ;


RPAREN
   : ')'
   ;


LBRACE
   : '{'
   ;


RBRACE
   : '}'
   ;


NGT
   : '\'>'
   ;


NLT
   : '\'<'
   ;


GT
   : '>'
   ;


LT
   : '<'
   ;


ADD
   : '+'
   ;


SUBTRACT
   : '-'
   ;


MULTIPLY
   : '*'
   ;


DIVIDE
   : '/'
   ;


INTDIVIDE
   : '\\'
   ;


MODULO
   : '#'
   ;


EXPONENT
   : '**'
   ;


EQUALS
   : '='
   ;


QUESTION
   : '?'
   ;


DOT
   : '.'
   ;


CONCAT
   : '_'
   ;


IDENTIFIER
   : ('a' .. 'z' | 'A' .. 'Z') ('a' .. 'z' | 'A' .. 'Z' | '0' .. '9')*
   ;


STRING_LITERAL
   : '"' ('""' | ~'"')* '"'
   ;


NUMBER
   : ('0' .. '9') + ('.' ('0' .. '9') +)?
   ;


SPACE
   : ' '
   ;


NOT
   : '\''
   ;


fragment A
   : ('a' | 'A')
   ;


fragment B
   : ('b' | 'B')
   ;


fragment C
   : ('c' | 'C')
   ;


fragment D
   : ('d' | 'D')
   ;


fragment E
   : ('e' | 'E')
   ;


fragment F
   : ('f' | 'F')
   ;


fragment G
   : ('g' | 'G')
   ;


fragment H
   : ('h' | 'H')
   ;


fragment I
   : ('i' | 'I')
   ;


fragment J
   : ('j' | 'J')
   ;


fragment K
   : ('k' | 'K')
   ;


fragment L
   : ('l' | 'L')
   ;


fragment M
   : ('m' | 'M')
   ;


fragment N
   : ('n' | 'N')
   ;


fragment O
   : ('o' | 'O')
   ;


fragment P
   : ('p' | 'P')
   ;


fragment Q
   : ('q' | 'Q')
   ;


fragment R
   : ('r' | 'R')
   ;


fragment S
   : ('s' | 'S')
   ;


fragment T
   : ('t' | 'T')
   ;


fragment U
   : ('u' | 'U')
   ;


fragment V
   : ('v' | 'V')
   ;


fragment W
   : ('w' | 'W')
   ;


fragment X
   : ('x' | 'X')
   ;


fragment Y
   : ('y' | 'Y')
   ;


fragment Z
   : ('z' | 'Z')
   ;


COMMENT
   : ';' ~[\r\n]* -> skip
   ;


CR
   : '\r'? '\n'
   ;


WS
   : [\t] -> skip
   ;
