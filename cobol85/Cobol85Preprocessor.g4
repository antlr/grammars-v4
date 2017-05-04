/*
* Copyright (C) 2016, Ulrich Wolffgang <u.wol@wwu.de>
* All rights reserved.
*
* This software may be modified and distributed under the terms
* of the BSD 3-clause license. See the LICENSE file for details.
*/

/*
* Cobol 85 Preprocessor Grammar for ANTLR4
*
* This is a preprocessor grammar for Cobol 85.
*/

grammar Cobol85Preprocessor;

startRule
   : (copyStatement | execCicsStatement | execSqlStatement | execSqlImsStatement | replaceOffStatement | replaceArea | charData | controlSpacingStatement)* EOF
   ;

// exec cics statement

execCicsStatement
   : EXEC CICS charData END_EXEC DOT?
   ;

// exec sql statement

execSqlStatement
   : EXEC SQL charData END_EXEC DOT?
   ;

// exec sql ims statement

execSqlImsStatement
   : EXEC SQLIMS charData END_EXEC DOT?
   ;

// copy statement

copyStatement
   : COPY copySource (NEWLINE* directoryPhrase)? (NEWLINE* familyPhrase)? (NEWLINE* replacingPhrase)? SUPPRESS? DOT
   ;

copySource
   : literal | cobolWord
   ;

replacingPhrase
   : REPLACING NEWLINE* replaceClause (NEWLINE+ replaceClause)*
   ;

// replace statement

replaceArea
   : replaceByStatement (copyStatement | charData)* replaceOffStatement?
   ;

replaceByStatement
   : REPLACE (NEWLINE* replaceClause)+ DOT
   ;

replaceOffStatement
   : REPLACE OFF DOT
   ;

replaceClause
   : replaceable NEWLINE* BY NEWLINE* replacement (NEWLINE* directoryPhrase)? (NEWLINE* familyPhrase)?
   ;

directoryPhrase
   : (OF | IN) NEWLINE* (literal | cobolWord)
   ;

familyPhrase
   : ON NEWLINE* (literal | cobolWord)
   ;

replaceable
   : literal | cobolWord | pseudoText | charDataLine
   ;

replacement
   : literal | cobolWord | pseudoText | charDataLine
   ;

controlSpacingStatement
   : SKIP1 | SKIP2 | SKIP3 | EJECT
   ;

// literal ----------------------------------

cobolWord
   : IDENTIFIER
   ;

literal
   : NONNUMERICLITERAL
   ;

pseudoText
   : DOUBLEEQUALCHAR charData? DOUBLEEQUALCHAR
   ;

charData
   : (charDataLine | NEWLINE)+
   ;

charDataLine
   : (charDataKeyword | cobolWord | literal | TEXT | DOT)+
   ;

// keywords ----------------------------------

charDataKeyword
   : BY | IN | OF | OFF | ON | REPLACING
   ;

// lexer rules --------------------------------------------------------------------------------

// keywords
BY : B Y;
CICS : C I C S;
COPY : C O P Y;
EJECT : E J E C T;
END_EXEC : E N D '-' E X E C;
EXEC : E X E C;
IN : I N;
OF : O F;
OFF : O F F;
ON : O N;
REPLACE : R E P L A C E;
REPLACING : R E P L A C I N G;
SQL : S Q L;
SQLIMS : S Q L I M S;
SKIP1 : S K I P '1';
SKIP2 : S K I P '2';
SKIP3 : S K I P '3';
SUPPRESS : S U P P R E S S;

// symbols
COMMENTTAG : '>*';
DOT : '.';
DOUBLEEQUALCHAR : '==';

// literals
NONNUMERICLITERAL : STRINGLITERAL | HEXNUMBER;

fragment HEXNUMBER :
	X '"' [0-9A-F]+ '"'
	| X '\'' [0-9A-F]+ '\''
;

fragment STRINGLITERAL :
	'"' (~["\n\r] | '""' | '\'')* '"'
	| '\'' (~['\n\r] | '\'\'' | '"')* '\''
;

IDENTIFIER : [a-zA-Z0-9]+ ([-_]+ [a-zA-Z0-9]+)*;

// whitespace, line breaks, comments, ...
NEWLINE : '\r'? '\n';
COMMENTLINE : COMMENTTAG ~('\n' | '\r')* -> channel(HIDDEN);
WS : [ \t\f;]+ -> channel(HIDDEN);
TEXT : ~('\n' | '\r');

// case insensitive chars
fragment A:('a'|'A');
fragment B:('b'|'B');
fragment C:('c'|'C');
fragment D:('d'|'D');
fragment E:('e'|'E');
fragment F:('f'|'F');
fragment G:('g'|'G');
fragment H:('h'|'H');
fragment I:('i'|'I');
fragment J:('j'|'J');
fragment K:('k'|'K');
fragment L:('l'|'L');
fragment M:('m'|'M');
fragment N:('n'|'N');
fragment O:('o'|'O');
fragment P:('p'|'P');
fragment Q:('q'|'Q');
fragment R:('r'|'R');
fragment S:('s'|'S');
fragment T:('t'|'T');
fragment U:('u'|'U');
fragment V:('v'|'V');
fragment W:('w'|'W');
fragment X:('x'|'X');
fragment Y:('y'|'Y');
fragment Z:('z'|'Z');