/*	
 * Fortran 77 grammar for ANTLR 2.7.5
 * Adadpted from Fortran 77 PCCTS grammar by Olivier Dragon
 * Original PCCTS grammar by Terence Parr
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA
 *
 */

/**
 * ported to Antlr4 by Tom Everett
 */

/*
 * Updated by Tom Everett, 2018
 */
lexer grammar Fortran77Lexer;


PROGRAM
   : 'program' | 'PROGRAM'
   ;


ENTRY
   : 'entry' | 'ENTRY'
   ;


FUNCTION
   : 'function' | 'FUNCTION'
   ;


BLOCK
   : 'block' | 'BLOCK'
   ;


SUBROUTINE
   : 'subroutine' | 'SUBROUTINE'
   ;


END
   : 'END' | 'end'
   ;


DIMENSION
   : 'dimension' | 'DIMENSION'
   ;


REAL
   : 'REAL' | 'real'
   ;


EQUIVALENCE
   : 'EQUIVALENCE' | 'equivalence'
   ;


COMMON
   : 'common' | 'COMMON'
   ;


POINTER
   : 'pointer' | 'POINTER'
   ;


IMPLICIT
   : 'implicit' | 'IMPLICIT'
   ;


NONE
   : 'none' | 'NONE'
   ;


CHARACTER
   : 'character' | 'CHARACTER'
   ;


PARAMETER
   : 'parameter' | 'PARAMETER'
   ;


EXTERNAL
   : 'external' | 'EXTERNAL'
   ;


INTRINSIC
   : 'intrinsic' | 'INTRINSIC'
   ;


SAVE
   : 'save' | 'SAVE'
   ;


DATA
   : 'data' | 'DATA'
   ;


GO
   : 'GO' | 'go'
   ;


GOTO
   : 'GOTO' | 'goto'
   ;


IF
   : 'IF' | 'if'
   ;


THEN
   : 'THEN' | 'then'
   ;


ELSE
   : 'ELSE' | 'else'
   ;


ENDIF
   : 'ENDIF' | 'endif'
   ;


ELSEIF
   : 'ELSEIF' | 'elseif'
   ;


DO
   : 'DO' | 'do'
   ;


CONTINUE
   : 'CONTINUE' | 'continue'
   ;


STOP
   : 'STOP' | 'stop'
   ;


ENDDO
   : 'ENDDO' | 'enddo'
   ;


PAUSE
   : 'pause' | 'PAUSE'
   ;


WRITE
   : 'WRITE' | 'write'
   ;


READ
   : 'READ' | 'read'
   ;


PRINT
   : 'PRINT' | 'print'
   ;


OPEN
   : 'OPEN' | 'open'
   ;


FMT
   : 'FMT' | 'fmt'
   ;


UNIT
   : 'UNIT' | 'unit'
   ;


ERR
   : 'err' | 'ERR'
   ;


IOSTAT
   : 'IOSTAT' | 'iostat'
   ;


FORMAT
   : 'FORMAT' | 'format'
   ;


LET
   : 'LET' | 'let'
   ;


CALL
   : 'CALL' | 'call'
   ;


RETURN
   : 'RETURN' | 'return'
   ;


CLOSE
   : 'CLOSE' | 'close'
   ;


DOUBLE
   : 'DOUBLE' | 'double'
   ;


IOSTART
   : 'IOSTART' | 'iostart'
   ;


SEQUENTIAL
   : 'SEQUENTIAL' | 'sequential'
   ;


LABEL
   : 'LABEL' | 'label'
   ;


FILE
   : 'file' | 'FILE'
   ;


STATUS
   : 'STATUS' | 'status'
   ;


ACCESS
   : 'ACCESS' | 'access'
   ;


POSITION
   : 'POSITION' | 'position'
   ;


FORM
   : 'FORM' | 'form'
   ;


RECL
   : 'RECL' | 'recl'
   ;


BLANK
   : 'BLANK' | 'blank'
   ;


EXIST
   : 'EXIST' | 'exist'
   ;


OPENED
   : 'OPENED' | 'opened'
   ;


NUMBER
   : 'NUMBER' | 'number'
   ;


NAMED
   : 'NAMED' | 'named'
   ;


NAME_
   : 'NAME' | 'name'
   ;


FORMATTED
   : 'FORMATTED' | 'formatted'
   ;


UNFORMATTED
   : 'UNFORMATTED' | 'unformatted'
   ;


NEXTREC
   : 'NEXTREC' | 'nextrec'
   ;


INQUIRE
   : 'INQUIRE' | 'inquire'
   ;


BACKSPACE
   : 'BACKSPACE' | 'backspace'
   ;


ENDFILE
   : 'ENDFILE' | 'endfile'
   ;


REWIND
   : 'REWIND' | 'rewind'
   ;


DOLLAR
   : '$'
   ;


COMMA
   : ','
   ;


LPAREN
   : '('
   ;


RPAREN
   : ')'
   ;


COLON
   : ':'
   ;


ASSIGN
   : '='
   ;


MINUS
   : '-'
   ;


PLUS
   : '+'
   ;


DIV
   : '/'
   ;

fragment STARCHAR
   : '*'
   ;



POWER
   : '**'
   ;


LNOT
   : '.not.' | '.NOT.'
   ;


LAND
   : '.and.' | '.AND.'
   ;


LOR
   : '.or.' | '.OR.'
   ;


EQV
   : '.eqv.' | '.EQV.'
   ;


NEQV
   : '.neqv.' | '.NEQV.'
   ;


XOR
   : '.xor.' | '.XOR.'
   ;


EOR
   : '.eor.' | '.EOR.'
   ;


LT
   : '.lt.' | '.LT.'
   ;


LE
   : '.le.' | '.LE.'
   ;


GT
   : '.gt.' | '.GT.'
   ;


GE
   : '.ge.' | '.GE.'
   ;


NE
   : '.ne.' | '.NE.'
   ;


EQ
   : '.eq.' | '.EQ.'
   ;


TRUE
   : '.true.' | '.TRUE.'
   ;


FALSE
   : '.false.' | '.FALSE.'
   ;


XCON
   : 'XCON'
   ;


PCON
   : 'PCON'
   ;


FCON
   : 'FCON'
   ;


CCON
   : 'CCON'
   ;


HOLLERITH
   : 'HOLLERITH'
   ;


CONCATOP
   : 'CONCATOP'
   ;


CTRLDIRECT
   : 'CTRLDIRECT'
   ;


CTRLREC
   : 'CTRLREC'
   ;


TO
   : 'TO' | 'to'
   ;


SUBPROGRAMBLOCK
   : 'SUBPROGRAMBLOCK'
   ;


DOBLOCK
   : 'DOBLOCK'
   ;


AIF
   : 'AIF'
   ;


THENBLOCK
   : 'THENBLOCK'
   ;


ELSEBLOCK
   : 'ELSEBLOCK'
   ;


CODEROOT
   : 'CODEROOT'
   ;


COMPLEX
   : 'COMPLEX' | 'complex'
   ;


PRECISION
   : 'PRECISION' | 'precision'
   ;


INTEGER
   : 'INTEGER' | 'integer'
   ;


LOGICAL
   : 'LOGICAL' | 'logical'
   ;


fragment CONTINUATION
   : ~ ('0' | ' ')
   ;


fragment ALNUM
   : (ALPHA | NUM)
   ;


fragment HEX
   : (NUM | 'a' .. 'f')
   ;


fragment SIGN
   : ('+' | '-')
   ;


fragment FDESC
   : ('i' | 'f' | 'd') (NUM) + '.' (NUM) + | ('e' | 'g') (NUM) + '.' (NUM) + ('e' (NUM) +)?
   ;


fragment EXPON
   : ('e' | 'E' | 'd' | 'D') (SIGN)? (NUM) +
   ;


fragment ALPHA
   : ('a' .. 'z') | ('A' .. 'Z')
   ;


fragment NUM
   : ('0' .. '9')
   ;

// '' is used to drop the charater when forming the lexical token
// Strings are assumed to start with a single quote (') and two
// single quotes is meant as a literal single quote

SCON
   : '\'' ('\'' '\'' | ~ ('\'' | '\n' | '\r') | (('\n' | '\r' ('\n')?) '     ' CONTINUATION) ('\n' | '\r' ('\n')?) '     ' CONTINUATION)* '\''
   ;

RCON
   : NUM+ '.' NUM* EXPON?
   ;

ICON
   : NUM+
   ;

NAME
   : (('i' | 'f' | 'd' | 'g' | 'e') (NUM) + '.') FDESC | (ALNUM +) (ALNUM)*
   ;


COMMENT
   : {getCharPositionInLine() == 0}? ('c' | STARCHAR) (~ [\r\n])* EOL -> channel(HIDDEN)
   ;

STAR
   : STARCHAR
   ;


STRINGLITERAL
   : '"' ~ ["\r\n]* '"'
   ;

EOL
   : [\r\n] +
   ;

LINECONT
   : ((EOL '     $') | (EOL '     +')) -> skip
   ;
   
WS
   : [\t ] + -> skip
   ;
