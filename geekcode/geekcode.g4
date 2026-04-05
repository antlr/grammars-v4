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

// $antlr-format alignTrailingComments true, columnLimit 150, minEmptyLines 1, maxEmptyLinesToKeep 1, reflowComments false, useTab false


// $antlr-format allowShortRulesOnASingleLine false, allowShortBlocksOnASingleLine true, alignSemicolons hanging, alignColons hanging

grammar geekcode;

file_
   : GCODE tuple+ EOF
   ;

tuple
   : REFUSE? tupletyepe (sizing | crossover | qualifier)*
   ;

crossover
   : (LPAREN sizing RPAREN)
   ;

sizing
   : ('-' | '+' | '*')+
   ;

tupletyepe
   : DRESS
   | shapetuple
   | AGE
   | HAIR
   | COMPUTERS
   | PERL
   | EMACS
   | WWW
   | USENET
   | USENET_ORACLE
   | KIBO
   | OS2
   | MACINTOSH
   | VMS
   | POLITCALSOCIAL
   | POLITICSECONOMIC
   | CYPHERPUNKS
   | PGP
   | STARTREK
   | BABYLON5
   | XFILES
   | ROLEPLAYING
   | TELEVISION
   | BOOKS
   | DILBERT
   | DOOM
   | EDUCATION
   | HOUSEING
   | RELATIONSHIPS
   | SEXU
   | SEXM
   | SEXF
   | LINUX
   | GLASSES
   | PENS
   | AUTO
   | WEIRDNESS
   | VERBAGE
   | BSD386
   | JEOPARDY
   | MAGIC
   | BARNEY
   | MUSIC
   | FRIENDS
   | NUTRITION
   | unixtuple
   ;
   // use 'U' bc UNIX and ULTRIX are both 'U'

unixtuple
   : UNIX (BSD | 'U' | AIX | SYSV | HPUX | IRIX | OSF1 | SOLARIS | SCO | NEXT | LINUX)+
   ;

shapetuple
   : 's' sizing? qualifier* (':' sizing? qualifier*)*
   ;

qualifier
   : NOTRIGID
   | WANNABE
   | FORLIVING
   | NOKNOWLEDGE
   ;

NOTRIGID
   : '@'
   ;

WANNABE
   : '>'
   ;

FORLIVING
   : '$'
   ;

NOKNOWLEDGE
   : '?'
   ;

REFUSE
   : '!'
   ;

LPAREN
   : '('
   ;

RPAREN
   : ')'
   ;

UNIX
   : 'U'
   ;

BSD
   : 'UB'
   ;

ULTRIX
   : 'UU'
   ;

AIX
   : 'UA'
   ;

SYSV
   : 'UV'
   ;

HPUX
   : 'UH'
   ;

IRIX
   : 'UI'
   ;

OSF1
   : 'UO'
   ;

SOLARIS
   : 'US'
   ;

SCO
   : 'UC'
   ;

NEXT
   : 'UX'
   ;

DRESS
   : 'd'
   ;

SHAPE
   : 's'
   ;

AGE
   : 'a'
   ;

COMPUTERS
   : 'C'
   ;

PERL
   : 'P'
   ;

LINUX
   : 'L'
   ;

EMACS
   : 'E'
   ;

WWW
   : 'W'
   ;

USENET
   : 'N'
   ;

USENET_ORACLE
   : 'o'
   ;

KIBO
   : 'K'
   ;

OS2
   : 'O'
   ;

MACINTOSH
   : 'M'
   ;

VMS
   : 'V'
   ;

POLITCALSOCIAL
   : 'PS'
   ;

POLITICSECONOMIC
   : 'PE'
   ;

CYPHERPUNKS
   : 'Y'
   ;

PGP
   : 'PGP'
   ;

STARTREK
   : 't'
   ;

BABYLON5
   : '5'
   ;

XFILES
   : 'X'
   ;

ROLEPLAYING
   : 'R'
   ;

TELEVISION
   : 'tv'
   ;

BOOKS
   : 'b'
   ;

DILBERT
   : 'DI'
   ;

DOOM
   : 'D'
   ;

EDUCATION
   : 'e'
   ;

HOUSEING
   : 'h'
   ;

RELATIONSHIPS
   : 'r'
   ;

SEXU
   : 'y'
   ;

SEXM
   : 'z'
   ;

SEXF
   : 'x'
   ;

HAIR
   : 'H'
   ;

GLASSES
   : 'g'
   ;

PENS
   : 'p'
   ;

AUTO
   : 'au'
   ;

WEIRDNESS
   : 'w'
   ;

VERBAGE
   : 'v'
   ;

BSD386
   : '3'
   ;

JEOPARDY
   : 'j'
   ;

MAGIC
   : 'G'
   ;

BARNEY
   : 'B'
   ;

MUSIC
   : 'u'
   ;

FRIENDS
   : 'f'
   ;

NUTRITION
   : 'n'
   ;

GCODE
   : 'G' ALPHA+ ('/' ALPHA+)*
   ;

ALPHA
   : [A-Z]
   ;

WS
   : [ \r\n\t]+ -> skip
   ;

