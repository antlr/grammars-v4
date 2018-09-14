/*
BSD License
Copyright (c) 2018, Tom Everett
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

lexer grammar powerbuilderLexer;


DATA_TYPE_SUB
   : (A N Y) | (B L O B) | (B O O L E A N) | (B Y T E) | (C H A R A C T E R) | (C H A R) | (D A T E) | (D A T E T I M E) | (D E C I M A L) | (D E C) | (D O U B L E) | (I N T E G E R) | (I N T) | (L O N G) | (L O N G L O N G) | (R E A L) | (S T R I N G) | (T I M E) | (U N S I G N E D I N T E G E R) | (U I N T) | (U N S I G N E D L O N G) | (U L O N G) | (W I N D O W)
   ;


BOOLEAN_ATOM
   : (T R U E) | (F A L S E)
   ;


GLOBAL
   : G L O B A L
   ;


SHARED
   : S H A R E D
   ;


END
   : E N D
   ;


INDIRECT
   : I N D I R E C T
   ;


VARIABLES
   : V A R I A B L E S
   ;


FORWARD
   : F O R W A R D
   ;


PUBLIC
   : P U B L I C
   ;


PRIVATE
   : P R I V A T E
   ;


FUNCTION
   : F U N C T I O N
   ;


SUBROUTINE
   : S U B R O U T I N E
   ;


READONLY
   : R E A D O N L Y
   ;


PROTOTYPES
   : P R O T O T Y P E S
   ;


TYPE
   : T Y P E
   ;


ON
   : O N
   ;


TO
   : T O
   ;


FROM
   : F R O M
   ;


REF
   : R E F
   ;


NULL
   : N U L L
   ;


UPDATE
   : U P D A T E
   ;


CASE
   : C A S E
   ;


DYNAMIC
   : D Y N A M I C
   ;


WITHIN
   : W I T H I N
   ;


PRIVATEWRITE
   : P R I V A T E W R I T E
   ;


PROTECTED
   : P R O T E C T E D
   ;


PRIVATEREAD
   : P R I V A T E R E A D
   ;


PROTECTEDREAD
   : P R O T E C T E D R E A D
   ;


PROTECTEDWRITE
   : P R O T E C T E D W R I T E
   ;


LOCAL
   : L O C A L
   ;


EVENT
   : E V E N T
   ;


OPEN
   : O P E N
   ;


GOTO
   : G O T O
   ;


ELSE
   : E L S E
   ;


IF
   : I F
   ;


THEN
   : T H E N
   ;


ELSEIF
   : E L S E I F
   ;


TRY
   : T R Y
   ;


EXIT
   : E X I T
   ;


CHOOSE
   : C H O O S E
   ;


IS
   : I S
   ;


CONTINUE
   : C O N T I N U E
   ;


DO
   : D O
   ;


WHILE
   : W H I L E
   ;


FOR
   : F O R
   ;


CLOSE
   : C L O S E
   ;


NEXT
   : N E X T
   ;


LOOP
   : L O O P
   ;


UNTIL
   : U N T I L
   ;


STEP
   : S T E P
   ;


CATCH
   : C A T C H
   ;


FINALLY
   : F I N A L L Y
   ;


THROW
   : T H R O W
   ;


RELEASE
   : R E L E A S E
   ;


CREATE
   : C R E A T E
   ;


DESTROY
   : D E S T R O Y
   ;


USING
   : U S I N G
   ;


POST
   : P O S T
   ;


TRIGGER
   : T R I G G E R
   ;


SELECT
   : S E L E C T
   ;


DELETE
   : D E L E T E
   ;


INSERT
   : I N S E R T
   ;


TIME2
   : T I M E
   ;


DESCRIBE
   : D E S C R I B E
   ;


RETURN
   : R E T U R N
   ;


OR
   : O R
   ;


AND
   : A N D
   ;


NOT
   : N O T
   ;


CALL
   : C A L L
   ;


HALT
   : H A L T
   ;


SUPER
   : S U P E R
   ;


LIBRARY
   : L I B R A R Y
   ;


SYSTEM
   : S Y S T E M
   ;


RPCFUNC
   : R P C F U N C
   ;


ALIAS
   : A L I A S
   ;


THROWS
   : T H R O W S
   ;


EQ
   : '='
   ;


GT
   : '>'
   ;


GTE
   : '>='
   ;


LT
   : '<'
   ;


LTE
   : '<='
   ;


GTLT
   : '<>'
   ;


PLUS
   : '+'
   ;


MINUS
   : '-'
   ;


PLUSEQ
   : '+='
   ;


MINUSEQ
   : '-='
   ;


COLONCOLON
   : '::'
   ;


MULT
   : '*'
   ;


DIV
   : '/'
   ;


MULTEQ
   : '*='
   ;


DIVEQ
   : '/='
   ;


CARAT
   : '^'
   ;


LCURLY
   : '{'
   ;


RCURLY
   : '}'
   ;


LBRACE
   : '['
   ;


RBRACE
   : ']'
   ;


BRACES
   : '[]'
   ;


TICK
   : '`'
   ;


AUTOINSTANTIATE
   : A U T O I N S T A N T I A T E
   ;


DESCRIPTOR
   : D E S C R I P T O R
   ;


DQUOTED_STRING
   : '"' (E_TILDE | ~ ('"') | E_DOUBLE_QUOTE)* '"'
   ;


QUOTED_STRING
   : '\'' (~ ('\'') | E_QUOTE)* '\''
   ;


fragment ID_PARTS
   : [a-zA-Z] ([a-zA-Z] | DIGIT | '-' | '$' | '#' | '%' | '_')*
   ;


ENUM
   : ID_PARTS '!'
   ;


COMMA
   : ','
   ;


ID
   : ID_PARTS
   ;


SEMI
   : ';'
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


NUMBER
   : ((NUM POINT NUM) | POINT NUM | NUM) ('E' ('+' | '-')? NUM)? ('D' | 'F')?
   ;


fragment NUM
   : DIGIT (DIGIT)*
   ;


DOT
   : POINT
   ;


fragment POINT
   : '.'
   ;


DQUOTE
   : '"'
   ;


fragment TAB
   : '~t'
   ;


fragment CR
   : '~r'
   ;


fragment LF
   : '~n'
   ;


fragment E_DOUBLE_QUOTE
   : '~"'
   ;


fragment E_QUOTE
   : '~\''
   ;


fragment E_TILDE
   : '~~'
   ;


fragment DIGIT
   : '0' .. '9'
   ;


DATE
   : DIGIT DIGIT DIGIT DIGIT '-' DIGIT DIGIT '-' DIGIT DIGIT
   ;


TIME
   : DIGIT DIGIT ':' DIGIT DIGIT ':' DIGIT DIGIT (':' DIGIT DIGIT DIGIT DIGIT DIGIT DIGIT)?
   ;


BINDPAR
   : ':' ID_PARTS
   ;


TQ
   : '???'
   ;


DOUBLE_PIPE
   : '||'
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


LINE_CONTINUATION
   : '&' WS* [\r\n] -> skip
   ;


DOTDOTDOT
   : '...'
   ;


fragment LETTER
   : 'A' .. 'Z' | 'a' .. 'z'
   ;


EXPORT_HEADER
   : '$' 'A' .. 'Z' ((LETTER | DIGIT | '-' | '#' | '%' | '_'))* '$' (LETTER | DIGIT | '.' | ' ') + ~ [\r\n]
   ;


SL_COMMENT
   : '//' ~ [\r\n]* -> skip
   ;


ML_COMMENT
   : '/*' (.)*? '*/' -> skip
   ;


WS
   : [ \t\r\n] + -> skip
   ;
