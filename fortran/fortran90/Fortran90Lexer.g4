lexer grammar Fortran90Lexer; 

options { superClass = Fortran90LexerBase; } 

// Insert here @header for C++ lexer.

RECURSIVE  
   : 'RECURSIVE' | 'recursive' |'Recursive' ; 
 
 
CONTAINS 
   : 
   ('contains'|'CONTAINS' |'Contains') 
   ; 
 
MODULE 
   : 'MODULE' | 'module' |'Module'
   ; 
 
ENDMODULE 
   : 'ENDMODULE' | 'endmodule' |'Endmodule'
   ; 
 
PROGRAM 
   : 'program' | 'PROGRAM' |'Program'
   ; 
 
ENTRY 
   : 'entry' | 'ENTRY' |'Entry'
   ; 
  
FUNCTION 
   : 'function' | 'FUNCTION' | 'Function'
   ; 
 
 
BLOCK 
   : 'block' | 'BLOCK' | 'Block'
   ; 
 
SUBROUTINE
   : 'subroutine' | 'SUBROUTINE' | 'Subroutine'
   ;

ENDINTERFACE
   : 'ENDINTERFACE' | ' endinterface' |'Endinterface'
   ;

PROCEDURE
   : 'procedure'  | 'PROCEDURE' | 'Procedure'
   ;

END
   : 'END' | 'end'| 'End'
   ;

DIMENSION
   : 'dimension' | 'DIMENSION' | 'Dimension'
   ;

TARGET : 'TARGET' | 'target' |'Target' ;

ALLOCATABLE : 'ALLOCATABLE' | 'allocatable' |'Allocatable' ;

OPTIONAL : 'OPTIONAL' | 'optional' |'Optional' ;

NAMELIST : 'NAMELIST' | 'namelist' ;

INTENT : 'INTENT' | 'intent' |'Intent' ;

IN : 'IN' | 'in' |'In' ;

OUT : 'OUT' | 'out' |'Out' ;

INOUT : 'INOUT' | 'inout' | 'Inout' ;

OPERATOR : 'operator' | 'OPERATOR' | 'Operator';

USE : 'USE' | 'use' |'Use' ;

ONLY : 'ONLY' | 'only' |'Only' ;

IMPLIEDT : '=>' ;

ASSIGNMENT : 'ASSIGNMENT' | 'assignment' |'Assignment' ;



DOP : '.''\\a'+'.';

OP 
   :'=='
   | '!='
   | '<='
   |'>='
   |'<'
   |'>'
   |'/='
   ; 

DOUBLEPRECISION : 'DOUBLEPRECISION' | 'doubleprecision' | 'double precision' | 'DOUBLE PRECISION';

DOUBLECOLON : '::' ;

ASSIGNSTMT : 'assign' | 'ASSIGN' |'Assign';

COMMON : 'COMMON' | 'common' |'Common';

ELSEWHERE : 'ELSEWHERE' | 'elsewhere' |'Elsewhere' ;

REAL
   : 'REAL' | 'real' |'Real'
   ;



EQUIVALENCE
   : 'EQUIVALENCE' | 'equivalence' |'Equivalence'
   ;


BLOCKDATA
   : 'blockdata' | 'BLOCKDATA' |'Blockdata'
   ;


POINTER
   : 'pointer' | 'POINTER' |'Pointer'
   ;

fragment PRIVATES
    : 'private' | 'PRIVATE' |'Private'
    ;

PRIVATE : PRIVATES ;

SEQUENCE
   : 'sequence' | 'SEQUENCE' |'Sequence'
   ;
   
fragment PUBLIC 
    : 'public' | 'PUBLIC' | 'Public'
    ;

ACCESSSPEC
   : PRIVATE | PUBLIC
   ;

IMPLICIT
   : 'implicit' | 'IMPLICIT' | 'Implicit'
   ;

NONE
   : 'none' | 'NONE' | 'None'
   ;

CHARACTER
   : 'character' | 'CHARACTER' | 'Character'
   ;


PARAMETER
   : 'parameter' | 'PARAMETER' | 'Parameter'
   ;


EXTERNAL
   : 'external' | 'EXTERNAL' | 'External'
   ;


INTRINSIC
   : 'intrinsic' | 'INTRINSIC' |'Intrinsic'
   ;


SAVE
   : 'save' | 'SAVE' |'Save'
   ;


DATA
   : 'data' | 'DATA' | 'Data'
   ;


GO
   : 'GO' | 'go'  | 'Go'
   ;


GOTO
   : 'GOTO' | 'goto' | 'Goto'
   ;


IF
   : 'IF' | 'if' | 'If'
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

RESULT
   : 'RESULT' | 'result'
   ;


ELSEIF
   : 'ELSEIF' | 'elseif' | 'Elseif'
   ;


DO
   : 'DO' | 'do' |'Do'
   ;

INCLUDE : 'INCLUDE' | 'include' |'Include' ;

CONTINUE
   : 'CONTINUE' | 'continue' |'Continue'
   ;

ENDWHERE : 'ENDWHERE' | 'endwhere' | 'Endwhere';

WHERE : 'WHERE' | 'where' |'Where' ;

ENDSELECT : 'ENDSELECT' | 'endselect' ;

SELECTCASE : 'SELECTCASE' | 'selectcase';

SELECT: 'SELECT' | 'select' ;

CASE : 'case' | 'CASE' |'Case' ;

DEFAULT : 'DEFAULT' | 'default' | 'Default';

DIRECT : 'DIRECT' | 'direct' |'Direct' ;

STOP
   : 'STOP' | 'stop' | 'Stop'
   ;

 REC : 'REC' | 'rec' |'Rec'
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

PAD : 'PAD' | 'pad' ;

ACTION : 'ACTION' | 'action' ;

DELIM : 'DELIM' | 'delim' ;

IOLENGTH : 'IOLENGTH' | 'iolength' ;

READWRITE : 'READWRITE' | 'readwrite' ;

ERR
   : 'err' | 'ERR'
   ;

SIZE : 'SIZE' | 'size' ;

ADVANCE : 'ADVANCE' | 'advance' ;

NML : 'NML' | 'nml' ;


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
   : 'RETURN' | 'return' | 'Return'
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

ENDBLOCKDATA : 'endblockdata' | 'ENDBLOCKDATA' ;

ENDBLOCK : 'ENDBLOCK' | 'endblock' ;




fragment NEWLINE
	: '\r\n' | '\r' | '\n'
	| '\u0085' // <Next Line CHARACTER (U+0085)>'
	| '\u2028' //'<Line Separator CHARACTER (U+2028)>'
	| '\u2029' //'<Paragraph Separator CHARACTER (U+2029)>'
	;
 

KIND : 'KIND' | 'kind' ;

LEN : 'LEN' | 'len' ;

//EOS : COMMENTORNEWLINE+ ;
//EOS : (COMMENTORNEWLINE? SPACES* [\r\n] [ \t]* )+;

//RN : NEWLINE -> channel(HIDDEN) ;

WS
   :  ([ \t]  | NEWLINE)+ -> channel(HIDDEN)
   ;

COMMENT
    : (('\t'* '\u0020'* '!' (~ [\r\n])* [\r\n]* )
    | ( {this.IsColumnZero()}? ('c'| 'C') (~ [\r\n])* [\r\n]*)) -> channel(HIDDEN) ;

/*
COMMENTORNEWLINE 
   : COMMENT
   |
   NEWLINE

   ;
*/



DOLLAR
   : '$'
   ;


COMMA
   : ','
   ;


LPAREN
   : '('
   ;

PCT : '%';

WHILE : 'while' | 'WHILE';

ALLOCATE : 'ALLOCATE' | 'allocate' ;

STAT : 'STAT' | 'stat';

RPAREN
   : ')'
   ;


COLON
   : ':'
   ;


SEMICOLON
   : ';'
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

FORMATSEP 
   : '/' | ':'
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
   : NUM+ [xX]
   ;


PCON
   : [+-]?NUM+[pP]
   ;


FCON
   : ('a'|'A'|'b'|'B'|'e'|'E'|'d'|'D'|(('e'|'E')('n'|'N'|'s'|'S'))|'q'|'Q'|'f'|'F'|'g'|'G'|'i'|'I'|'l'|'L'|'o'|'O'|'z'|'Z')(NUM+|'*')('.'NUM+(('e'|'E'|'d'|'D'|'q'|'Q')NUM+)?)
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
   : 'TO'
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
   : 'INTEGER' | 'integer' |'Integer'
   ;


LOGICAL 
   : 'LOGICAL' | 'logical' | 'Logical'
   ;

fragment SCORE : '_';

UNDERSCORE : SCORE ;

OBRACKETSLASH : '(/';

DOT : '.' ;

CBRACKETSLASH : '/)';

ZCON :  [zZ]'\''[abcdefABCDEF0-9] + '\''
   | [zZ] '"'[abcdefABCDEF0-9] +'"' ;

BCON : [bB]'\'' [01] + '\''
   | [bB]'"'[01] + '"'
   ;

 OCON 
   : [oO]'"'[01234567] + '"'
   | [oO]'\''[01234567] + '\''
   ;

SCON
   : '\'' ('\'' '\'' | ~ ('\'' | '\n' | '\r') | (('\n' | '\r' ('\n')?) '     ' CONTINUATION) ('\n' | '\r' ('\n')?) '     ' CONTINUATION)* '\''
   |
   '\'' (~('\'') | ('\'''\''))*  ('\'' )
   |
   ('"') (~('"') | '""')*  ('"')
   ;

RDCON : NUM+ '.' NUM* EXPON? 
      | NUM* '.' NUM+ EXPON?
      | NUM+ EXPON
      ;

DEALLOCATE : 'DEALLOCATE' | 'deallocate' ;

NULLIFY : 'NULLIFY' | 'nullify' ;

CYCLE : 'CYCLE' | 'cycle' ;
   
ENDTYPE : 'ENDTYPE' | 'endtype' | 'Endtype' |'EndType';

INTERFACE : 'INTERFACE' | 'interface' | 'Interface' ;
   
SPOFF : 'SPOFF';

SPON : 'SPON';

ICON
   : NUM+
   ;

TYPE 
   : 'type' | 'TYPE' | 'Type'
   ;

NAME
   :LETTER ( ALPHANUMERIC_CHARACTER )*
   ;

EXIT : 'EXIT' | 'exit' ;

BLANK
   : 'BLANK' | 'blank'
   ;


ALPHANUMERIC_CHARACTER : LETTER | NUM | SCORE ;

fragment LETTER : ('a'..'z' | 'A'..'Z') ;


STAR
   : STARCHAR
   ;



STRINGLITERAL
   : '"' ~ ["\r\n]* '"'
   ;

   
EOL
   : [\r\n] +
   ;

fragment SPACES
 : [ \t] +
 ;

LINECONT
   :  (('&' SPACES? COMMENT? NEWLINE (SPACES* [ \t] * '&' )?) | ( SPACES? COMMENT? NEWLINE SPACES* [ \t] * '&' )) -> channel(HIDDEN)
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

