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

grammar pdb;

file_
   : record (EOL record)* EOL? EOF
   ;

record
   : HEADER
   | TITLE
   | AUTHOR
   | REMARK
   | SEQRES
   | ATOM
   | HETATM
   | metadata
   | structure
   | coordinates
   | connectivity
   | bookkeeping
   ;

metadata
   : METADATA
   ;

structure
   : STRUCTURE
   ;

coordinates
   : COORDINATES
   ;

connectivity
   : CONNECTIVITY
   ;

bookkeeping
   : BOOKKEEPING
   ;

HEADER: 'HEADER' PAYLOAD?;
TITLE: 'TITLE ' PAYLOAD? | 'TITLE';
AUTHOR: 'AUTHOR' PAYLOAD?;
REMARK: 'REMARK' PAYLOAD?;
SEQRES: 'SEQRES' PAYLOAD?;
ATOM: 'ATOM  ' PAYLOAD? | 'ATOM';
HETATM: 'HETATM' PAYLOAD?;

METADATA
   : ('OBSLTE' | 'SPLIT ' | 'CAVEAT' | 'COMPND' | 'SOURCE' | 'KEYWDS'
     | 'EXPDTA' | 'NUMMDL' | 'MDLTYP' | 'REVDAT' | 'SPRSDE' | 'JRNL  ') PAYLOAD?
   ;

STRUCTURE
   : ('DBREF ' | 'DBREF1' | 'DBREF2' | 'SEQADV' | 'MODRES' | 'HET   '
     | 'HETNAM' | 'HETSYN' | 'FORMUL' | 'HELIX ' | 'SHEET ' | 'SSBOND'
     | 'LINK  ' | 'CISPEP' | 'SITE  ' | 'CRYST1' | 'ORIGX1' | 'ORIGX2'
     | 'ORIGX3' | 'SCALE1' | 'SCALE2' | 'SCALE3' | 'MTRIX1' | 'MTRIX2'
     | 'MTRIX3') PAYLOAD?
   ;

COORDINATES
   : ('MODEL ' | 'ANISOU' | 'TER   ') PAYLOAD?
   | 'ENDMDL' SPACE*
   | 'TER'
   ;

CONNECTIVITY
   : 'CONECT' PAYLOAD?
   ;

BOOKKEEPING
   : 'MASTER' PAYLOAD?
   | 'END' SPACE*
   ;

fragment PAYLOAD
   : ~[\r\n\t]+
   ;

fragment SPACE
   : ' '
   ;

EOL
   : '\r'? '\n'
   ;
