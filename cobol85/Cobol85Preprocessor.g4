/*
* Copyright (C) 2017, Ulrich Wolffgang <u.wol@wwu.de>
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
   : (compilerOptions | copyStatement | execCicsStatement | execSqlStatement | execSqlImsStatement | replaceOffStatement | replaceArea | ejectStatement | skipStatement | titleStatement | charDataLine | NEWLINE)* EOF
   ;

// compiler options

compilerOptions
   : (PROCESS | CBL) compilerOption (COMMACHAR? compilerOption)*
   ;

compilerOption
   : APOST | ARITH LPARENCHAR EXTEND RPARENCHAR | AWO
   | CBLCARD | CICS | COBOL2 | COBOL3 | CODEPAGE LPARENCHAR literal RPARENCHAR | COMPILE | CPP | CPSM
   | DATA LPARENCHAR literal RPARENCHAR | DBCS | DEBUG
   | EDF | EPILOG
   | FASTSRT | FEPI | FLAG LPARENCHAR cobolWord RPARENCHAR
   | GDS | GRAPHIC
   | LANGUAGE LPARENCHAR (CS | EN | KA) RPARENCHAR | LEASM | LENGTH | LIB | LIN | LINECOUNT LPARENCHAR literal RPARENCHAR | LINKAGE
   | MAP | MARGINS LPARENCHAR literal COMMACHAR literal (COMMACHAR literal)? RPARENCHAR
   | NATLANG LPARENCHAR (CS | EN | KA) RPARENCHAR
   | NOADV
   | NOCBLCARD | NOCMPR2 | NOCPSM
   | NODBCS | NODEBUG | NODECK | NODUMP
   | NOEDF | NOEPILOG
   | NOFEPI | NOFLAGMIG | NOFLAGSTD
   | NOGRAPHIC
   | NOLENGTH | NOLINKAGE
   | NONAME | NONUM | NONUMBER
   | NOOPSEQUENCE | NOOPTIONS
   | NOP | NOPROLOG
   | NOS | NOSEQ | NOSEQUENCE | NOSOURCE | NOSPIE | NOSSRANGE | NOSTDTRUNC
   | NOTERM | NOTEST
   | NOVBREF
   | NOWORD
   | NSEQ
   | NOVBREF
   | NOXREF
   | NUM | NUMPROC LPARENCHAR NOPFD RPARENCHAR
   | OBJECT | OFFSET | OPMARGINS LPARENCHAR literal COMMACHAR literal (COMMACHAR literal)? RPARENCHAR | OPSEQUENCE LPARENCHAR literal COMMACHAR literal RPARENCHAR | OPTIMIZE LPARENCHAR FULL RPARENCHAR | OP | OPTIONS | OUTDD LPARENCHAR PPTDBG RPARENCHAR
   | PROLOG
   | QUOTE
   | RENT
   | SEQ LPARENCHAR literal COMMACHAR literal RPARENCHAR | SEQUENCE LPARENCHAR literal COMMACHAR literal RPARENCHAR | SOURCE | SP | SPACE LPARENCHAR literal RPARENCHAR | SPIE | SYSEIB
   | TRUNC LPARENCHAR STD RPARENCHAR
   | VBREF
   | XOPTS LPARENCHAR compilerOption (COMMACHAR? compilerOption)* RPARENCHAR
   | ZWB
   ;

// exec cics statement

execCicsStatement
   : EXEC CICS charData END_EXEC DOT?
   ;

// exec sql statement

execSqlStatement
   : EXEC SQL charDataSql END_EXEC DOT?
   ;

// exec sql ims statement

execSqlImsStatement
   : EXEC SQLIMS charData END_EXEC DOT?
   ;

// copy statement

copyStatement
   : COPY copySource (NEWLINE* (directoryPhrase | familyPhrase | replacingPhrase | SUPPRESS))* NEWLINE* DOT
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

// eject statement

ejectStatement
   : EJECT DOT?
   ;

// skip statement

skipStatement
   : (SKIP1 | SKIP2 | SKIP3) DOT?
   ;

// title statement

titleStatement
   : TITLE literal DOT?
   ;

// literal ----------------------------------

cobolWord
   : IDENTIFIER
   ;

literal
   : NONNUMERICLITERAL | NUMERICLITERAL
   ;

pseudoText
   : DOUBLEEQUALCHAR charData? DOUBLEEQUALCHAR
   ;

charData
   : (charDataLine | NEWLINE)+
   ;

charDataSql
   : (charDataLine | REPLACE | NEWLINE)+
   ;

charDataLine
   : (charDataKeyword | cobolWord | literal | TEXT | DOT)+
   ;

// keywords ----------------------------------

charDataKeyword
   : APOST | ARITH | AWO
   | BY
   | CBL | CBLCARD | COBOL2 | COBOL3 | CODEPAGE | COMMACHAR | COMPILE | CPP | CPSM | CS
   | DATA | DBCS | DEBUG | DLI
   | EDF | EJECT | EN | EPILOG | EXCI | EXTEND
   | FASTSRT | FLAG | FULL
   | GDS | GRAPHIC
   | IN
   | KA
   | LANGUAGE | LENGTH | LIB | LIN | LINECOUNT | LINKAGE | LPARENCHAR
   | MAP | MARGINS
   | NATLANG
   | NOADV
   | NOCBLCARD | NOCMPR2 | NOCPSM
   | NODBCS | NODEBUG | NODECK | NODUMP
   | NOEDF | NOEPILOG
   | NOFEPI | NOFLAGMIG | NOFLAGSTD
   | NOGRAPHIC
   | NOLENGTH | NOLINKAGE
   | NONAME | NONUM | NONUMBER
   | NOOPSEQUENCE | NOOPTIONS
   | NOP | NOPFD | NOPROLOG
   | NOS | NOSEQ | NOSEQUENCE | NOSOURCE | NOSPIE | NOSSRANGE | NOSTDTRUNC
   | NOTERM | NOTEST
   | NOVBREF
   | NOWORD
   | NOXREF
   | NSEQ
   | NUM | NUMPROC
   | OBJECT | ON | OF | OFF | OFFSET | OPMARGINS | OPSEQUENCE | OPTIMIZE | OP | OPTIONS | OUTDD
   | PPTDBG | PROCESS | PROLOG
   | QUOTE
   | RENT | REPLACING | RPARENCHAR
   | SEQ | SEQUENCE | SOURCE | SP | SPACE | SPIE | STD | SYSEIB
   | TITLE | TRUNC
   | VBREF
   | XOPTS
   | ZWB
   ;

// lexer rules --------------------------------------------------------------------------------

// keywords
APOST : A P O S T;
ARITH : A R I T H;
AWO : A W O;
BY : B Y;
CBL : C B L;
CBLCARD : C B L C A R D;
CICS : C I C S;
COBOL2 : C O B O L '2';
COBOL3 : C O B O L '3';
CODEPAGE : C O D E P A G E;
COMPILE : C O M P I L E;
CPP : C P P;
CPSM : C P S M;
CS : C S;
COPY : C O P Y;
DATA : D A T A;
DBCS : D B C S;
DEBUG : D E B U G;
DLI : D L I;
EDF : E D F;
EJECT : E J E C T;
EN : E N;
END_EXEC : E N D '-' E X E C;
EPILOG : E P I L O G;
EXCI : E X C I;
EXEC : E X E C;
EXTEND : E X T E N D;
FASTSRT : F A S T S R T;
FEPI : F E P I;
FLAG : F L A G;
FULL : F U L L;
GDS : G D S;
GRAPHIC : G R A P H I C;
IN : I N;
KA : K A;
LANGUAGE : L A N G U A G E;
LEASM : L E A S M;
LENGTH : L E N G T H;
LIB : L I B;
LIN : L I N;
LINECOUNT : L I N E C O U N T;
LINKAGE : L I N K A G E;
LPARENCHAR : '(';
MAP : M A P;
MARGINS : M A R G I N S;
NATLANG : N A T L A N G;
NOADV : N O A D V;
NOCBLCARD : N O C B L C A R D;
NOCMPR2 : N O C M P R '2';
NOCPSM : N O C P S M;
NODBCS : N O D B C S;
NODEBUG : N O D E B U G;
NODECK : N O D E C K;
NODUMP : N O D U M P;
NOEDF : N O E D F;
NOEPILOG : N O E P I L O G;
NOFEPI : N O F E P I;
NOFLAGMIG : N O F L A G M I G;
NOFLAGSTD : N O F L A G S T D;
NOGRAPHIC : N O G R A P H I C;
NOLENGTH : N O L E N G T H;
NOLINKAGE : N O L I N K A G E;
NONAME : N O N A M E;
NONUM : N O N U M;
NONUMBER : N O N U M B E R;
NOOPSEQUENCE : N O O P S E Q U E N C E;
NOOPTIONS : N O O P T I O N S;
NOP : N O P;
NOPFD : N O P F D;
NOPROLOG : N O P R O L O G;
NOS : N O S;
NOSEQ : N O S E Q;
NOSOURCE : N O S O U R C E;
NOSPIE : N O S P I E;
NOSSRANGE : N O S S R A N G E;
NOSTDTRUNC : N O S T D T R U N C;
NOSEQUENCE : N O S E Q U E N C E;
NOTERM : N O T E R M;
NOTEST : N O T E S T;
NOVBREF : N O V B R E F;
NOWORD : N O W O R D;
NOXREF : N O X R E F;
NSEQ : N S E Q;
NUM : N U M;
NUMPROC : N U M P R O C;
OBJECT : O B J E C T;
OF : O F;
OFF : O F F;
OFFSET : O F F S E T;
ON : O N;
OP : O P;
OPMARGINS : O P M A R G I N S;
OPSEQUENCE : O P S E Q U E N C E;
OPTIMIZE : O P T I M I Z E;
OPTIONS : O P T I O N S;
OUTDD : O U T D D;
PPTDBG : P P T D B G;
PROCESS : P R O C E S S;
PROLOG : P R O L O G;
QUOTE : Q U O T E;
RENT : R E N T;
REPLACE : R E P L A C E;
REPLACING : R E P L A C I N G;
RPARENCHAR : ')';
SEQ : S E Q;
SEQUENCE : S E Q U E N C E;
SOURCE : S O U R C E;
SP : S P;
SPACE : S P A C E;
SPIE : S P I E;
SQL : S Q L;
SQLIMS : S Q L I M S;
SKIP1 : S K I P '1';
SKIP2 : S K I P '2';
SKIP3 : S K I P '3';
STD : S T D;
SUPPRESS : S U P P R E S S;
SYSEIB : S Y S E I B;
TITLE : T I T L E;
TRUNC : T R U N C;
VBREF : V B R E F;
XOPTS: X O P T S;
ZWB : Z W B;

// symbols
COMMENTTAG : '*>';
COMMACHAR : ',';
DOT : '.';
DOUBLEEQUALCHAR : '==';

// literals
NONNUMERICLITERAL : STRINGLITERAL | HEXNUMBER;
NUMERICLITERAL : [0-9]+;

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