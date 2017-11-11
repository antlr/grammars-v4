/*
BSD License
Copyright (c) 2017, Tom Everett
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

grammar smiles;

smiles 
    : chain terminator
    ;

atom
   : bracket_atom
   | aliphatic_organic
   | aromatic_organic
   | '*'
   ;

aliphatic_organic
   : 'B'
   | 'C'
   | 'N'
   | 'O'
   | 'S'
   | 'P'
   | 'F'
   | 'Cl'
   | 'Br'
   | 'I'
   ;

aromatic_organic
   : 'b'
   | 'c'
   | 'n'
   | 'o'
   | 's'
   | 'p'
   ;

bracket_atom
   : '[' isotope? symbol chiral? hcount? charge? class_? ']'
   ;

symbol
   : element_symbols
   | aromatic_symbols
   | '*'
   ;

isotope
   : NUMBER
   ;

element_symbols
   : 'H'
   | 'He'
   | 'Li'
   | 'Be'
   | 'B'
   | 'C'
   | 'N'
   | 'O'
   | 'F'
   | 'Ne'
   | 'Na'
   | 'Mg'
   | 'Al'
   | 'Si'
   | 'P'
   | 'S'
   | 'Cl'
   | 'Ar'
   | 'K'
   | 'Ca'
   | 'Sc'
   | 'Ti'
   | 'V'
   | 'Cr'
   | 'Mn'
   | 'Fe'
   | 'Co'
   | 'Ni'
   | 'Cu'
   | 'Zn'
   | 'Ga'
   | 'Ge'
   | 'As'
   | 'Se'
   | 'Br'
   | 'Kr'
   | 'Rb'
   | 'Sr'
   | 'Y'
   | 'Zr'
   | 'Nb'
   | 'Mo'
   | 'Tc'
   | 'Ru'
   | 'Rh'
   | 'Pd'
   | 'Ag'
   | 'Cd'
   | 'In'
   | 'Sn'
   | 'Sb'
   | 'Te'
   | 'I'
   | 'Xe'
   | 'Cs'
   | 'Ba'
   | 'Hf'
   | 'Ta'
   | 'W'
   | 'Re'
   | 'Os'
   | 'Ir'
   | 'Pt'
   | 'Au'
   | 'Hg'
   | 'Tl'
   | 'Pb'
   | 'Bi'
   | 'Po'
   | 'At'
   | 'Rn'
   | 'Fr'
   | 'Ra'
   | 'Rf'
   | 'Db'
   | 'Sg'
   | 'Bh'
   | 'Hs'
   | 'Mt'
   | 'Ds'
   | 'Rg'
   | 'La'
   | 'Ce'
   | 'Pr'
   | 'Nd'
   | 'Pm'
   | 'Sm'
   | 'Eu'
   | 'Gd'
   | 'Tb'
   | 'Dy'
   | 'Ho'
   | 'Er'
   | 'Tm'
   | 'Yb'
   | 'Lu'
   | 'Ac'
   | 'Th'
   | 'Pa'
   | 'U'
   | 'Np'
   | 'Pu'
   | 'Am'
   | 'Cm'
   | 'Bk'
   | 'Cf'
   | 'Es'
   | 'Fm'
   | 'Md'
   | 'No'
   | 'Lr'
   ;

aromatic_symbols
   : 'c'
   | 'n'
   | 'o'
   | 'p'
   | 's'
   | 'se'
   | 'as'
   ;

chiral
   : '@'
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
   ;

hcount
   : 'H'
   | 'H' DIGIT
   ;

charge
   : '-'
   | '-' DIGIT
   | '+'
   | '+' DIGIT
   | '--'
   | '++'
   ;

class_
   : ':' NUMBER
   ;

bond
   : '-'
   | '='
   | '#'
   | '$'
   | ':'
   | '/'
   | '\\'
   ;

ringbond
   : bond? DIGIT
   | bond? '%' DIGIT DIGIT
   ;

branched_atom
   : atom ringbond* branch*
   ;

branch
   : '(' chain ')'
   | '(' bond chain ')'
   | '(' DOT chain ')'
   ;

chain
   : branched_atom
   | chain branched_atom
   | chain bond branched_atom
   | chain DOT branched_atom
   ;

terminator
   : SPACE TAB
   | LINEFEED
   | CARRIAGE_RETURN
   ;

DOT
   : '.'
   ;


LINEFEED
   : '\r'
   ;

CARRIAGE_RETURN
   : '\n'
   ;

SPACE
   : ' '
   ;

DIGIT
   : '0' .. '9'
   ;

NUMBER
   : DIGIT +
   ;

TAB
   : '\t'
   ;
