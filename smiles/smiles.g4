/*
 BSD License Copyright (c) 2017, Tom Everett All rights reserved. Redistribution and use in source
 and binary forms, with or without modification, are permitted provided that the following
 conditions are met: 1. Redistributions of source code must retain the above copyright notice, this
 list of conditions and the following disclaimer. 2. Redistributions in binary form must reproduce
 the above copyright notice, this list of conditions and the following disclaimer in the
 documentation and/or other materials provided with the distribution. 3. Neither the name of Tom
 Everett nor the names of its contributors may be used to endorse or promote products derived from
 this software without specific prior written permission. THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT
 HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED
 TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED.
 IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
 INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
 PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
 INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
 OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE,
 EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */
grammar smiles;

smiles: chain terminator? EOF;

atom: bracket_atom | aliphatic_organic | aromatic_organic | '*';

aliphatic_organic:
	UB
	| UC
	| UN
	| UO
	| US
	| UP
	| UF
	| UC LL
	| UB LR
	| UI;

aromatic_organic: LB | LC | LN | LO | LS | LP;

bracket_atom:
	'[' isotope? symbol chiral? hcount? charge? class_? ']';

isotope: DIGIT+;

symbol: element_symbols | aromatic_symbol | '*';

element_symbols: UH
	| UH LE
	| UL LI
	| UB LE
	| UB
	| UC
	| UN
	| UO
	| UF
	| UN LE
	| UN LA
	| UM LG
	| UA LL
	| US LI
	| UP
	| US
	| UC LL
	| UA LR
	| UK
	| UC LA
	| US LC
	| UT LI
	| UV
	| UC LR
	| UM LN
	| UF LE
	| UC LO
	| UN LI
	| UC LU
	| UZ LN
	| UG LA
	| UG LE
	| UA LS
	| US LE
	| UB LR
	| UK LR
	| UR LB
	| US LR
	| UY
	| UZ LR
	| UN LB
	| UM LO
	| UT LC
	| UR LU
	| UR LH
	| UP LD
	| UA LG
	| UC LD
	| UI LN
	| US LN
	| US LB
	| UT LE
	| UI
	| UX LE
	| UC LS
	| UB LA
	| UH LF
	| UT LA
	| UW
	| UR LE
	| UO LS
	| UI LR
	| UP LT
	| UA LU
	| UH LG
	| UT LL
	| UP LB
	| UB LI
	| UP LO
	| UA LT
	| UR LN
	| UF LR
	| UR LA
	| UR LF
	| UD LB
	| US LG
	| UB LH
	| UH LS
	| UM LT
	| UD LS
	| UR LG
	| UC LN
	| UF LL
	| UL LV
	| UL LA
	| UC LE
	| UP LR
	| UN LD
	| UP LM
	| US LM
	| UE LU
	| UG LD
	| UT LB
	| UD LY
	| UH LO
	| UE LR
	| UT LM
	| UY LB
	| UL LU
	| UA LC
	| UT LH
	| UP LA
	| UU
	| UN LP
	| UP LU
	| UA LM
	| UC LM
	| UB LK
	| UC LF
	| UE LS
	| UF LM
	| UM LD
	| UN LO
	| UL LR;

aromatic_symbol: LC | LN | LO | LP | LS | LS LE | LA LS;

chiral:
	'@'
	| '@@'
	| '@TH1'
	| '@TH2'
	| '@AL1'
	| '@AL2'
	| '@SP1'
	| '@SP2'
	| '@SP3'
	| '@TB1'
	| '@TB2'
	| '@TB3'
	| '@TB3'
	| '@TB4'
	| '@TB5'
	| '@TB6'
	| '@TB7'
	| '@TB8'
	| '@TB9'
	| '@TB10'
	| '@TB11'
	| '@TB12'
	| '@TB13'
	| '@TB14'
	| '@TB15'
	| '@TB16'
	| '@TB17'
	| '@TB18'
	| '@TB19'
	| '@TB20'
	| '@TB21'
	| '@TB22'
	| '@TB23'
	| '@TB24'
	| '@TB25'
	| '@TB26'
	| '@TB27'
	| '@TB28'
	| '@TB29'
	| '@TB30'
	| '@OH1'
	| '@OH2'
	| '@OH3'
	| '@OH4'
	| '@OH5'
	| '@OH6'
	| '@OH7'
	| '@OH8'
	| '@OH9'
	| '@OH10'
	| '@OH11'
	| '@OH12'
	| '@OH13'
	| '@OH14'
	| '@OH15'
	| '@OH16'
	| '@OH17'
	| '@OH18'
	| '@OH19'
	| '@OH20'
	| '@OH21'
	| '@OH22'
	| '@OH23'
	| '@OH24'
	| '@OH25'
	| '@OH26'
	| '@OH27'
	| '@OH28'
	| '@OH29'
	| '@OH30'
	| ('@TB' | '@OH') DIGIT DIGIT;

hcount: UH DIGIT?;

charge: ('+' | '-') DIGIT? DIGIT? | '--' | '++';

class_: ':' DIGIT+;

bond: '-' | '=' | '#' | '$' | ':' | '/' | '\\';

ringbond: bond? (DIGIT | '%' DIGIT DIGIT);

branched_atom: atom ringbond* branch*;

branch: '(' (bond | DOT)? chain ')';

chain: branched_atom ( (bond | DOT)? branched_atom)*;

terminator: SPACE TAB | LINEFEED | CARRIAGE_RETURN;

LA: 'a';

LB: 'b';

LC: 'c';

LD: 'd';

LE: 'e';

LF: 'f';

LG: 'g';

LH: 'h';

LI: 'i';

LJ: 'j';

LK: 'k';

LL: 'l';

LM: 'm';

LN: 'n';

LO: 'o';

LP: 'p';

LQ: 'q';

LR: 'r';

LS: 's';

LT: 't';

LU: 'u';

LV: 'v';

LW: 'w';

LX: 'x';

LY: 'y';

LZ: 'z';

UA: 'A';

UB: 'B';

UC: 'C';

UD: 'D';

UE: 'E';

UF: 'F';

UG: 'G';

UH: 'H';

UI: 'I';

UJ: 'J';

UK: 'K';

UL: 'L';

UM: 'M';

UN: 'N';

UO: 'O';

UP: 'P';

UQ: 'Q';

UR: 'R';

US: 'S';

UT: 'T';

UU: 'U';

UV: 'V';

UW: 'W';

UX: 'X';

UY: 'Y';

UZ: 'Z';

DOT: '.';

LINEFEED: '\r';

CARRIAGE_RETURN: '\n';

SPACE: ' ';

DIGIT: '0' .. '9';

TAB: '\t';
