/*
* Copyright (C) 2015 Ulrich Wolffgang <u.wol@wwu.de>
*
* This program is free software: you can redistribute it and/or modify
* it under the terms of the GNU Lesser General Public License as 
* published by the Free Software Foundation, either version 3 of the 
* License, or (at your option) any later version.
* 
* This program is distributed in the hope that it will be useful,
* but WITHOUT ANY WARRANTY; without even the implied warranty of
* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
* GNU Lesser General Public License for more details.
* 
* You should have received a copy of the GNU Lesser General Public License
* along with this program. If not, see <http://www.gnu.org/licenses/>.
*/

/*
* Cobol 85 Preprocessor Grammar for ANTLR4
*
* This is a preprocessor grammar for Cobol 85.
*
* Change log:
*
* v1.0
*	- EXEC SQL
*	- EXEC CICS
*
* v0.9 Initial revision
*/

grammar Cobol85Preprocessor;

options
{
	language = Java;
}

startRule : (
	copyStatement
	| execCicsStatement
	| execSqlStatement
	| replaceOffStatement
	| replaceArea 
	| charData
)* EOF;


// exec cics statemen

execCicsStatement :
	EXEC CICS charData END_EXEC
;


// exec sql statement

execSqlStatement :
	EXEC SQL charData END_EXEC
;


// copy statement

copyStatement :
	COPY copySource
	(NEWLINE* directoryPhrase)?
	(NEWLINE* familyPhrase)?
	(NEWLINE* replacingPhrase)?
	DOT
;

copySource : literal | cobolWord;

replacingPhrase :
	REPLACING NEWLINE* replaceClause (NEWLINE+ replaceClause)*
;


// replace statement

replaceArea : 
	replaceByStatement
	(copyStatement | charData)*
	replaceOffStatement
;

replaceByStatement :
	REPLACE (NEWLINE* replaceClause)+ DOT
;

replaceOffStatement :
	REPLACE OFF DOT
;


replaceClause : 
	replaceable NEWLINE* BY NEWLINE* replacement
	(NEWLINE* directoryPhrase)? 
	(NEWLINE* familyPhrase)? 
;

directoryPhrase :
	(OF | IN) NEWLINE* (literal | cobolWord)
;

familyPhrase : 
	ON NEWLINE* (literal | cobolWord)
;

replaceable : literal | cobolWord | pseudoText | charDataLine;

replacement : literal | cobolWord | pseudoText | charDataLine;


// literal ----------------------------------

cobolWord : IDENTIFIER;

literal : NONNUMERICLITERAL;

pseudoText : DOUBLEEQUALCHAR charData? DOUBLEEQUALCHAR;

charData : 
	(
		charDataLine
		| NEWLINE
	)+
;

charDataLine : 
	(
		charDataKeyword
		| cobolWord
		| literal
		| TEXT
		| DOT
	)+
;


// keywords ----------------------------------

charDataKeyword : 
	BY
	| IN
	| OF | OFF | ON
	| REPLACING
;


// lexer rules --------------------------------------------------------------------------------

// keywords
BY : B Y;
CICS : C I C S;
COPY : C O P Y;
END_EXEC : E N D '-' E X E C; 
EXEC : E X E C;
IN : I N;
OF : O F;
OFF : O F F;
ON : O N;
REPLACE : R E P L A C E;
REPLACING : R E P L A C I N G;
SQL : S Q L;


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