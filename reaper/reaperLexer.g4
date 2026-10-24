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

/*
* https://esolangs.org/wiki/Reaper
*
* Reaper uses Python-like significant indentation to delimit the body of a
* class definition. INDENT/DEDENT/SEP tokens are synthesized by a custom
* lexer superclass (ReaperLexerBase, one implementation per supported
* target language, under Java/ and CSharp/), the same technique this repo's
* python3 and gdscript grammars use. That technique requires a split
* lexer/parser grammar, since `options { superClass = ...; }` only takes
* effect on the Lexer class when declared in a standalone `lexer grammar`
* file (in a combined grammar it only affects the Parser class).
*/
lexer grammar reaperLexer;

options {
   superClass = ReaperLexerBase;
}

tokens {
   INDENT,
   DEDENT,
   SEP
}

OPAREN
   : '('
   ;

CPAREN
   : ')'
   ;

ASSIGN
   : ':='
   ;

REPLACE
   : '='
   ;

NUMBER
   : [0-9]+
   ;

STRING
   : '"' ~ '"'* '"'
   ;

IDENTIFIER
   : [a-zA-Z] [a-zA-Z0-9.\-_]*
   ;

COMMENT
   : '--' ~ [\r\n]* -> skip
   ;

NEWLINE
   : ('\r'? '\n' [ \t]*) { onNewline(); }
   ;

WS
   : [ \t]+ -> skip
   ;
