
grammar jvmBasic;

/*
[The "BSD licence"]
Copyright (c) 2012 Tom Everett
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions
are met:
1. Redistributions of source code must retain the above copyright
notice, this list of conditions and the following disclaimer.
2. Redistributions in binary form must reproduce the above copyright
notice, this list of conditions and the following disclaimer in the
documentation and/or other materials provided with the distribution.
3. The name of the author may not be used to endorse or promote products
derived from this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE AUTHOR ``AS IS'' AND ANY EXPRESS OR
IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES
OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED.
IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY DIRECT, INDIRECT,
INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT
NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF
THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*/// a program is a collection of lines
prog
   : line + EOF
   ;

// a line starts with an INT
line
   : (linenumber ((amprstmt (COLON amprstmt?)*) | (COMMENT | REM)))
   ;

amperoper
   : AMPERSAND
   ;

linenumber
   : NUMBER
   ;

amprstmt
   : (amperoper? statement)
   | (COMMENT | REM)
   ;

statement
   : (CLS | LOAD | SAVE | TRACE | NOTRACE | FLASH | INVERSE | GR | NORMAL | SHLOAD | CLEAR | RUN | STOP | TEXT | HOME | HGR | HGR2)
   | endstmt
   | returnstmt
   | restorestmt
   | amptstmt
   | popstmt
   | liststmt
   | storestmt
   | getstmt
   | recallstmt
   | nextstmt
   | instmt
   | prstmt
   | onerrstmt
   | hlinstmt
   | vlinstmt
   | colorstmt
   | speedstmt
   | scalestmt
   | rotstmt
   | hcolorstmt
   | himemstmt
   | lomemstmt
   | printstmt1
   | pokestmt
   | plotstmt
   | ongotostmt
   | ongosubstmt
   | ifstmt
   | forstmt1
   | forstmt2
   | inputstmt
   | tabstmt
   | dimstmt
   | gotostmt
   | gosubstmt
   | callstmt
   | readstmt
   | hplotstmt
   | vplotstmt
   | vtabstmnt
   | htabstmnt
   | waitstmt
   | datastmt
   | xdrawstmt
   | drawstmt
   | defstmt
   | letstmt
   | includestmt
   ;

vardecl
   : var (LPAREN exprlist RPAREN)*
   ;

printstmt1
   : (PRINT | QUESTION) printlist?
   ;

printlist
   : expression ((COMMA | SEMICOLON) expression?)*
   ;

getstmt
   : GET exprlist
   ;

letstmt
   : LET? variableassignment
   ;

variableassignment
   : vardecl EQ exprlist
   ;

relop
   : (GTE)
   | (GT EQ)
   | (EQ GT)
   | LTE
   | (LT EQ)
   | (EQ LT)
   | neq
   | EQ
   | GT
   | LT
   ;

neq
   : LT GT
   ;

ifstmt
   : IF expression THEN? (statement | linenumber)
   ;

// for stmt 1 puts the for-next on one line
forstmt1
   : FOR vardecl EQ expression TO expression (STEP expression)? (statement NEXT vardecl?)?
   ;

// for stmt 2 puts the for, the statment, and the next on 3 lines.  It needs "nextstmt"
forstmt2
   : FOR vardecl EQ expression TO expression (STEP expression)?
   ;

nextstmt
   : NEXT (vardecl (',' vardecl)*)?
   ;

inputstmt
   : INPUT (STRINGLITERAL (COMMA | SEMICOLON))? varlist
   ;

readstmt
   : READ varlist
   ;

dimstmt
   : DIM varlist
   ;

gotostmt
   : GOTO linenumber
   ;

gosubstmt
   : GOSUB expression
   ;

pokestmt
   : POKE expression COMMA expression
   ;

callstmt
   : CALL exprlist
   ;

hplotstmt
   : HPLOT (expression COMMA expression)? (TO expression COMMA expression)*
   ;

vplotstmt
   : VPLOT (expression COMMA expression)? (TO expression COMMA expression)*
   ;

plotstmt
   : PLOT expression COMMA expression
   ;

ongotostmt
   : ON expression GOTO linenumber (COMMA linenumber)*
   ;

ongosubstmt
   : ON expression GOSUB linenumber (COMMA linenumber)*
   ;

vtabstmnt
   : VTAB expression
   ;

htabstmnt
   : HTAB expression
   ;

himemstmt
   : HIMEM COLON expression
   ;

lomemstmt
   : LOMEM COLON expression
   ;

datastmt
   : DATA datum (COMMA datum?)*
   ;

datum
   : number
   | STRINGLITERAL
   ;

waitstmt
   : WAIT expression COMMA expression (COMMA expression)?
   ;

xdrawstmt
   : XDRAW expression (AT expression COMMA expression)?
   ;

drawstmt
   : DRAW expression (AT expression COMMA expression)?
   ;

defstmt
   : DEF FN? var LPAREN var RPAREN EQ expression
   ;

tabstmt
   : TAB LPAREN expression RPAREN
   ;

speedstmt
   : SPEED EQ expression
   ;

rotstmt
   : ROT EQ expression
   ;

scalestmt
   : SCALE EQ expression
   ;

colorstmt
   : COLOR EQ expression
   ;

hcolorstmt
   : HCOLOR EQ expression
   ;

hlinstmt
   : HLIN expression COMMA expression AT expression
   ;

vlinstmt
   : VLIN expression COMMA expression AT expression
   ;

onerrstmt
   : ONERR GOTO linenumber
   ;

prstmt
   : PRNUMBER NUMBER
   ;

instmt
   : INNUMBER NUMBER
   ;

storestmt
   : STORE vardecl
   ;

recallstmt
   : RECALL vardecl
   ;

liststmt
   : LIST expression?
   ;

popstmt
   : POP (expression COMMA expression)?
   ;

amptstmt
   : AMPERSAND expression
   ;

includestmt
   : INCLUDE expression
   ;

endstmt
   : END
   ;

returnstmt
   : RETURN
   ;

restorestmt
   : RESTORE
   ;

// expressions and such
number
   :  ('+' | '-')? (NUMBER | FLOAT)
   ;

func
   : STRINGLITERAL
   | number
   | tabfunc
   | vardecl
   | chrfunc
   | sqrfunc
   | lenfunc
   | strfunc
   | ascfunc
   | scrnfunc
   | midfunc
   | pdlfunc
   | peekfunc
   | intfunc
   | spcfunc
   | frefunc
   | posfunc
   | usrfunc
   | leftfunc
   | valfunc
   | rightfunc
   | fnfunc
   | sinfunc
   | cosfunc
   | tanfunc
   | atnfunc
   | rndfunc
   | sgnfunc
   | expfunc
   | logfunc
   | absfunc
   | (LPAREN expression RPAREN)
   ;

signExpression
   : NOT? (PLUS | MINUS)? func
   ;

exponentExpression
   : signExpression (EXPONENT signExpression)*
   ;

multiplyingExpression
   : exponentExpression ((TIMES | DIV) exponentExpression)*
   ;

addingExpression
   : multiplyingExpression ((PLUS | MINUS) multiplyingExpression)*
   ;

relationalExpression
   : addingExpression ((relop) addingExpression)?
   ;

expression
   : func
   | (relationalExpression ((AND | OR) relationalExpression)*)
   ;

// lists
var
   : varname varsuffix?
   ;

varname
   : LETTERS (LETTERS | NUMBER)*
   ;

varsuffix
   : (DOLLAR | PERCENT)
   ;

varlist
   : vardecl (COMMA vardecl)*
   ;

exprlist
   : expression (COMMA expression)*
   ;

// functions
sqrfunc
   : SQR LPAREN expression RPAREN
   ;

chrfunc
   : CHR LPAREN expression RPAREN
   ;

lenfunc
   : LEN LPAREN expression RPAREN
   ;

ascfunc
   : ASC LPAREN expression RPAREN
   ;

midfunc
   : MID LPAREN expression COMMA expression COMMA expression RPAREN
   ;

pdlfunc
   : PDL LPAREN expression RPAREN
   ;

peekfunc
   : PEEK LPAREN expression RPAREN
   ;

intfunc
   : INTF LPAREN expression RPAREN
   ;

spcfunc
   : SPC LPAREN expression RPAREN
   ;

frefunc
   : FRE LPAREN expression RPAREN
   ;

posfunc
   : POS LPAREN expression RPAREN
   ;

usrfunc
   : USR LPAREN expression RPAREN
   ;

leftfunc
   : LEFT LPAREN expression COMMA expression RPAREN
   ;

rightfunc
   : RIGHT LPAREN expression COMMA expression RPAREN
   ;

strfunc
   : STR LPAREN expression RPAREN
   ;

fnfunc
   : FN var LPAREN expression RPAREN
   ;

valfunc
   : VAL LPAREN expression RPAREN
   ;

scrnfunc
   : SCRN LPAREN expression COMMA expression RPAREN
   ;

sinfunc
   : SIN LPAREN expression RPAREN
   ;

cosfunc
   : COS LPAREN expression RPAREN
   ;

tanfunc
   : TAN LPAREN expression RPAREN
   ;

atnfunc
   : ATN LPAREN expression RPAREN
   ;

rndfunc
   : RND LPAREN expression RPAREN
   ;

sgnfunc
   : SGN LPAREN expression RPAREN
   ;

expfunc
   : EXP LPAREN expression RPAREN
   ;

logfunc
   : LOG LPAREN expression RPAREN
   ;

absfunc
   : ABS LPAREN expression RPAREN
   ;

tabfunc
   : TAB LPAREN expression RPAREN
   ;


DOLLAR
   : '$'
   ;


PERCENT
   : '%'
   ;


RETURN
   : 'RETURN' | 'return'
   ;


PRINT
   : 'PRINT' | 'print'
   ;


GOTO
   : 'GOTO' | 'goto'
   ;


GOSUB
   : 'GOSUB' | 'gosub'
   ;


IF
   : 'IF' | 'if'
   ;


NEXT
   : 'NEXT' | 'next'
   ;


THEN
   : 'THEN' | 'then'
   ;


REM
   : 'REM' | 'rem'
   ;


CHR
   : 'CHR$'
   ;


MID
   : 'MID$'
   ;


LEFT
   : 'LEFT$'
   ;


RIGHT
   : 'RIGHT$'
   ;


STR
   : 'STR$'
   ;


LPAREN
   : '('
   ;


RPAREN
   : ')'
   ;


PLUS
   : '+'
   ;


MINUS
   : '-'
   ;


TIMES
   : '*'
   ;


DIV
   : '/'
   ;


CLEAR
   : 'CLEAR' | 'clear'
   ;


GTE
   : '>: '
   ;


LTE
   : '<: '
   ;


GT
   : '>'
   ;


LT
   : '<'
   ;


COMMA
   : ','
   ;


LIST
   : 'LIST' | 'list'
   ;


RUN
   : 'RUN' | 'run'
   ;


END
   : 'END' | 'end'
   ;


LET
   : 'LET' | 'let'
   ;


EQ
   : '='
   ;


FOR
   : 'FOR' | 'for'
   ;


TO
   : 'TO' | 'to'
   ;


STEP
   : 'STEP' | 'step'
   ;


INPUT
   : 'INPUT' | 'input'
   ;


SEMICOLON
   : ';'
   ;


DIM
   : 'DIM' | 'dim'
   ;


SQR
   : 'SQR' | 'sqr'
   ;


COLON
   : ':'
   ;


TEXT
   : 'TEXT' | 'text'
   ;


HGR
   : 'HGR' | 'hgr'
   ;


HGR2
   : 'HGR2' | 'hgr2'
   ;


LEN
   : 'LEN' | 'len'
   ;


CALL
   : 'CALL' | 'call'
   ;


ASC
   : 'ASC' | 'asc'
   ;


HPLOT
   : 'HPLOT' | 'hplot'
   ;


VPLOT
   : 'VPLOT' | 'vplot'
   ;


PRNUMBER
   : 'PR#'
   ;


INNUMBER
   : 'IN#'
   ;


VTAB
   : 'VTAB' | 'vtab'
   ;


HTAB
   : 'HTAB' | 'htab'
   ;


HOME
   : 'HOME' | 'home'
   ;


ON
   : 'ON' | 'on'
   ;


PDL
   : 'PDL' | 'pdl'
   ;


PLOT
   : 'PLOT' | 'plot'
   ;


PEEK
   : 'PEEK' | 'peek'
   ;


POKE
   : 'POKE' | 'poke'
   ;


INTF
   : 'INT' | 'int'
   ;


STOP
   : 'STOP' | 'stop'
   ;


HIMEM
   : 'HIMEM' | 'himem'
   ;


LOMEM
   : 'LOMEM' | 'lomem'
   ;


FLASH
   : 'FLASH' | 'flash'
   ;


INVERSE
   : 'INVERSE' | 'inverse'
   ;


NORMAL
   : 'NORMAL' | 'normal'
   ;


ONERR
   : 'ONERR' | 'onerr'
   ;


SPC
   : 'SPC' | 'spc'
   ;


FRE
   : 'FRE' | 'fre'
   ;


POS
   : 'POS' | 'pos'
   ;


USR
   : 'USR' | 'usr'
   ;


TRACE
   : 'TRACE' | 'trace'
   ;


NOTRACE
   : 'NOTRACE' | 'notrace'
   ;


AND
   : 'AND' | 'and'
   ;


OR
   : 'OR' | 'or'
   ;


DATA
   : 'DATA' | 'data'
   ;


WAIT
   : 'WAIT' | 'wait'
   ;


READ
   : 'READ' | 'read'
   ;


XDRAW
   : 'XDRAW' | 'xdraw'
   ;


DRAW
   : 'DRAW' | 'draw'
   ;


AT
   : 'AT' | 'at'
   ;


DEF
   : 'DEF' | 'def'
   ;


FN
   : 'FN' | 'fn'
   ;


VAL
   : 'VAL' | 'val'
   ;


TAB
   : 'TAB' | 'tab'
   ;


SPEED
   : 'SPEED' | 'speed'
   ;


ROT
   : 'ROT' | 'rot'
   ;


SCALE
   : 'SCALE' | 'scale'
   ;


COLOR
   : 'COLOR' | 'color'
   ;


HCOLOR
   : 'HCOLOR' | 'hcolor'
   ;


HLIN
   : 'HLIN' | 'hlin'
   ;


VLIN
   : 'VLIN' | 'vlin'
   ;


SCRN
   : 'SCRN' | 'scrn'
   ;


POP
   : 'POP' | 'pop'
   ;


SHLOAD
   : 'SHLOAD' | 'shload'
   ;


SIN
   : 'SIN' | 'sin'
   ;


COS
   : 'COS' | 'cos'
   ;


TAN
   : 'TAN' | 'tan'
   ;


ATN
   : 'ATN' | 'atn'
   ;


RND
   : 'RND' | 'rnd'
   ;


SGN
   : 'SGN' | 'sgn'
   ;


EXP
   : 'EXP' | 'exp'
   ;


LOG
   : 'LOG' | 'log'
   ;


ABS
   : 'ABS' | 'abs'
   ;


STORE
   : 'STORE' | 'store'
   ;


RECALL
   : 'RECALL' | 'recall'
   ;


GET
   : 'GET' | 'get'
   ;


EXPONENT
   : '^'
   ;


AMPERSAND
   : '&'
   ;


GR
   : 'GR' | 'gr'
   ;


NOT
   : 'NOT' | 'not'
   ;


RESTORE
   : 'RESTORE' | 'restore'
   ;


SAVE
   : 'SAVE' | 'save'
   ;


LOAD
   : 'LOAD' | 'load'
   ;


QUESTION
   : '?'
   ;


INCLUDE
   : 'INCLUDE' | 'include'
   ;


CLS
   : 'CLS' | 'cls'
   ;


COMMENT
   : REM ~ [\r\n]*
   ;


STRINGLITERAL
   : '"' ~ ["\r\n]* '"'
   ;


LETTERS
   : ('a' .. 'z' | 'A' .. 'Z') +
   ;


NUMBER
   : ('0' .. '9') + (('e' | 'E') NUMBER)*
   ;


FLOAT
   : ('0' .. '9')* '.' ('0' .. '9') + (('e' | 'E') ('0' .. '9') +)*
   ;


WS
   : [ \r\n\t] + -> channel (HIDDEN)
   ;
