grammar m2pim4_LL1; // Modula-2 PIM 4 standard

// file version 1.00, July 10, 2009

// Note: an empty semantic action {} is used in lists of alternative terminals
// in order to make ANTLRworks display the alternatives as separate branches.

// strict LL(1)

options {
	backtrack = no;
	k = 1;
	buildAST=true;
}

// Reserved Words

tokens {
	AND            = 'AND';
	ARRAY          = 'ARRAY';
	BEGIN          = 'BEGIN';
	BY             = 'BY';
	CASE           = 'CASE';
	CONST          = 'CONST';
	DEFINITION     = 'DEFINITION';
	DIV            = 'DIV';
	DO             = 'DO';
	ELSE           = 'ELSE';
	ELSIF          = 'ELSIF';
	END            = 'END';
	EXIT           = 'EXIT';
	EXPORT         = 'EXPORT';
	FOR            = 'FOR';
	FROM           = 'FROM';
	IF             = 'IF';
	IMPLEMENTATION = 'IMPLEMENTATION';
	IMPORT         = 'IMPORT';
	IN             = 'IN';
	LOOP           = 'LOOP';
	MOD            = 'MOD';
	MODULE         = 'MODULE';
	NOT            = 'NOT';
	OF             = 'OF';
	OR             = 'OR';
	POINTER        = 'POINTER';
	PROCEDURE      = 'PROCEDURE';
	QUALIFIED      = 'QUALIFIED';
	RECORD         = 'RECORD';
	REPEAT         = 'REPEAT';
	RETURN         = 'RETURN';
	SET            = 'SET';
	THEN           = 'THEN';
	TO             = 'TO';
	TYPE           = 'TYPE';
	UNTIL          = 'UNTIL';
	VAR            = 'VAR';
	WHILE          = 'WHILE';
	WITH           = 'WITH';
}

// ---------------------------------------------------------------------------
// L E X E R   G R A M M A R
// ---------------------------------------------------------------------------

// ***** PIM 4 Appendix 1 line 1 *****

IDENT :
	LETTER ( LETTER | DIGIT )*
	;

// ***** PIM 4 Appendix 1 lines 3-4 *****

INTEGER :
	DIGIT+ |
	OCTAL_DIGIT+  ( 'B' | 'C' {}) |
	DIGIT ( HEX_DIGIT )* 'H'
	;

// ***** PIM 4 Appendix 1 line 5 *****

REAL :
	DIGIT+ '.' DIGIT* SCALE_FACTOR?
	;

// ***** PIM 4 Appendix 1 line 10 *****

// Nore, the formal definition of string in PIM 4 does not match
//       the plain English description of string in the text.
//       => changed to match textual description
STRING :
	'\'' ( CHARACTER | '\"' )* '\'' | '"' (CHARACTER | '\'')* '"'
	;

// ***** PIM 4 provides no formal definition for letter *****

fragment
LETTER :
	'A' .. 'Z' | 'a' .. 'z'
	{} // make ANTLRworks display separate branches
	;

// ***** PIM 4 Appendix 1 line 11 *****

fragment
DIGIT :
	OCTAL_DIGIT | '8' | '9'
	{} // make ANTLRworks display separate branches
	;

// ***** PIM 4 Appendix 1 line 9 *****

fragment
OCTAL_DIGIT : '0' .. '7' ;

// ***** PIM 4 Appendix 1 line 7 *****

fragment
HEX_DIGIT :
	DIGIT | 'A' | 'B' | 'C' | 'D' | 'E' | 'F'
	{} // make ANTLRworks display separate branches
	;

// ***** PIM 4 Appendix 1 line 6 *****

fragment
SCALE_FACTOR :
	'E' ( '+' | '-' {})? DIGIT+
	;

// ***** PIM 4 provides no formal definition for character *****

fragment
CHARACTER :
	DIGIT | LETTER |
	// any printable characters other than single and double quote
	' ' | '!' | '#' | '$' | '%' | '&' | '(' | ')' | '*' | '+' |
	',' | '-' | '.' | ':' | ';' | '<' | '=' | '>' | '?' | '@' |
	'[' | '\\' | ']' | '^' | '_' | '`' | '{' | '|' | '}' | '~'
	{} // make ANTLRworks display separate branches
	;

// ---------------------------------------------------------------------------
// P A R S E R   G R A M M A R
// ---------------------------------------------------------------------------

// ***** PIM 4 Appendix 1 line 1 *****

ident :	IDENT ; // see lexer

// ***** PIM 4 Appendix 1 line 2 *****

number : INTEGER | REAL ; // see lexer

// ***** PIM 4 Appendix 1 lines 3-4 *****

integer : INTEGER ; // see lexer

// ***** PIM 4 Appendix 1 line 5 *****

real : REAL ; // see lexer

// ***** PIM 4 Appendix 1 line 6 *****

scaleFactor : SCALE_FACTOR ; // see lexer

// ***** PIM 4 Appendix 1 line 7 *****

hexDigit : HEX_DIGIT ; // see lexer

// ***** PIM 4 Appendix 1 line 8 *****

digit : DIGIT ; // see lexer

// ***** PIM 4 Appendix 1 line 9 *****

octalDigit : OCTAL_DIGIT ; // see lexer

// ***** PIM 4 Appendix 1 line 10 *****

string : STRING ; // see lexer

// ***** PIM 4 Appendix 1 line 11 *****

qualident :
	ident ( '.' ident )*
	;

// ***** PIM 4 Appendix 1 line 12 *****

constantDeclaration :	
	ident '=' constExpression
	;

// ***** PIM 4 Appendix 1 line 13 *****

constExpression :
	simpleConstExpr ( relation simpleConstExpr )?
	;

// ***** PIM 4 Appendix 1 line 14 *****

relation :
	'=' | '#' | '<>' | '<' | '<=' | '>' | '>=' | 'IN' {}
	;

// ***** PIM 4 Appendix 1 line 15 *****

simpleConstExpr :
	( '+' | '-' {})? constTerm ( addOperator constTerm )*
	;

// ***** PIM 4 Appendix 1 line 16 *****

addOperator :
	'+' | '-' | OR
	{} // make ANTLRworks display separate branches
	;

// ***** PIM 4 Appendix 1 line 17 *****

constTerm :
	constFactor ( mulOperator constFactor )*
	;

// ***** PIM 4 Appendix 1 line 18 *****

mulOperator :
	'*' | '/' | DIV | MOD | AND | '&'
	{} // make ANTLRworks display separate branches
	;

// ***** PIM 4 Appendix 1 lines 19-20 *****

// refactored for LL(1)
//
// Note: PIM 4 text says '~' is a synonym for 'NOT'
//       but the grammar does not actually show it
constFactor :
	number | string | setOrQualident |
	'(' constExpression ')' | ( NOT | '~' {}) constFactor
	;

// new for LL(1)
setOrQualident :
	set | qualident set?
	;

// ***** PIM 4 Appendix 1 line 21 *****

// refactored for LL(1)
set :
	/* qualident has been factored out */
	'{' ( element ( ',' element )* )? '}'
	;

// ***** PIM 4 Appendix 1 line 22 *****

element :
	constExpression ( '..' constExpression )?
	;

// ***** PIM 4 Appendix 1 line 23 *****

typeDeclaration :
	ident '=' type
	;

// ***** PIM 4 Appendix 1 lines 24-25 *****

type :
	simpleType | arrayType | recordType | setType | pointerType | procedureType
	;

// ***** PIM 4 Appendix 1 line 26 *****

simpleType :
	qualident | enumeration | subrangeType
	;

// ***** PIM 4 Appendix 1 line 27 *****

enumeration :
	'(' identList ')'
	;

// ***** PIM 4 Appendix 1 line 28 *****

identList :
	ident ( ',' ident )*
	;

// ***** PIM 4 Appendix 1 line 29 *****

subrangeType :
	'[' constExpression '..' constExpression ']'
	;

// ***** PIM 4 Appendix 1 line 30 *****

arrayType :
	ARRAY simpleType ( ',' simpleType )* OF type
	;

// ***** PIM 4 Appendix 1 line 31 *****

recordType :
	RECORD fieldListSequence END
	;

// ***** PIM 4 Appendix 1 line 32 *****

fieldListSequence :
	fieldList ( ';' fieldList )*
	;

// ***** PIM 4 Appendix 1 lines 33-35 *****

// refactored for LL(1)
fieldList :
	( identList ':' type |
	  CASE ident ( ( ':' | '.' {}) qualident )? OF variant ( '|' variant )*
	  ( ELSE fieldListSequence )?
	  END )?
	;

// ***** PIM 4 Appendix 1 line 36 *****

variant :
	caseLabelList ':' fieldListSequence
	;

// ***** PIM 4 Appendix 1 line 37 *****

caseLabelList :
	caseLabels ( ',' caseLabels )*
	;

// ***** PIM 4 Appendix 1 line 38 *****

caseLabels :
	constExpression ( '..' constExpression )?
	;

// ***** PIM 4 Appendix 1 line 39 *****

setType :
	SET OF simpleType
	;

// ***** PIM 4 Appendix 1 line 40 *****

pointerType :
	POINTER TO type
	;

// ***** PIM 4 Appendix 1 line 41 *****

procedureType :
	PROCEDURE formalTypeList?
	;

// ***** PIM 4 Appendix 1 lines 42-43 *****

formalTypeList :
	'(' ( VAR? formalType ( ',' VAR? formalType )* )? ')'
	( ':' qualident )?
	;

// ***** PIM 4 Appendix 1 line 44 *****

variableDeclaration :
	identList ':' type
	;

// ***** PIM 4 Appendix 1 line 45 *****

// refactored for LL(1)
designator :
	qualident ( designatorTail )?
	;

// new for LL(1)
designatorTail :
	( ( '[' expList ']' | '^' ) ( '.' ident )* )+
	;

// ***** PIM 4 Appendix 1 line 46 *****

expList :
	expression ( ',' expression )*
	;

// ***** PIM 4 Appendix 1 line 47 *****

expression :
	simpleExpression ( relation simpleExpression )?
	;

// ***** PIM 4 Appendix 1 line 48 *****

simpleExpression :
	( '+' | '-' {})? term ( addOperator term )*
	;

// ***** PIM 4 Appendix 1 line 49 *****

term :
	factor ( mulOperator factor )*
	;

// ***** PIM 4 Appendix 1 lines 50-51 *****

// refactored for LL(1)
//
// Note: PIM 4 text says '~' is a synonym for 'NOT'
//       but the grammar does not actually show it
factor :
	number |
	string |
	setOrDesignatorOrProcCall |
	'(' expression ')' | ( NOT | '~' {}) factor
	;

// new for LL(1)
setOrDesignatorOrProcCall :
	set |
	qualident /* <= factored out */
	( set | designatorTail? actualParameters? )
	;

// ***** PIM 4 Appendix 1 line 52 *****

actualParameters :
	'(' expList? ')'
	;

// ***** PIM 4 Appendix 1 lines 53-56 *****

// refactored for LL(1)
statement :
	( assignmentOrProcCall | ifStatement | caseStatement |
	  whileStatement | repeatStatement | loopStatement | forStatement |
	  withStatement | EXIT | RETURN expression? )?
	;

// ***** PIM 4 Appendix 1 line 57 *****

// and

// ***** PIM 4 Appendix 1 line 58 *****

// both replaced by

// new for LL(1)
assignmentOrProcCall :
	designator /* has been factored out */
	( ':=' expression | actualParameters? )
	;

// ***** PIM 4 Appendix 1 line 59 *****

statementSequence :
	statement ( ';' statement )*
	;

// ***** PIM 4 Appendix 1 lines 60-62 *****

ifStatement :
	IF expression THEN statementSequence
	( ELSIF expression THEN statementSequence )*
	( ELSE statementSequence )?
	END
	;

// ***** PIM 4 Appendix 1 lines 63-64 *****

caseStatement :
	CASE expression OF case ( '|' case )*
	( ELSE statementSequence )?
	END
	;

// ***** PIM 4 Appendix 1 line 65 *****

case :
	caseLabelList ':' statementSequence
	;

// ***** PIM 4  Appendix 1 line 66 *****

whileStatement :
	WHILE expression DO statementSequence END
	;

// ***** PIM 4 Appendix 1 line 67 *****

repeatStatement :
	REPEAT statementSequence UNTIL expression
	;

// ***** PIM 4 Appendix 1 lines 68-69 *****

forStatement :
	FOR ident ':=' expression TO expression ( BY constExpression )?
	DO statementSequence END
	;

// ***** PIM 4 Appendix 1 line 70 *****

loopStatement :
	LOOP statementSequence END
	;

// ***** PIM 4 Appendix 1 line 71 *****

withStatement :
	WITH designator DO statementSequence END
	;

// ***** PIM 4 Appendix 1 line 72 *****

procedureDeclaration :
	procedureHeading ';' block ident
	;

// ***** PIM 4 Appendix 1 line 73 *****

procedureHeading :
	PROCEDURE ident formalParameters?
	;

// ***** PIM 4 Appendix 1 line 74 *****

block :
	declaration*
	( BEGIN statementSequence )? END
	;

// ***** PIM 4 Appendix 1 lines 75-78 *****

declaration :
	CONST ( constantDeclaration ';' )* |
	TYPE ( typeDeclaration ';' )* |
	VAR ( variableDeclaration ';' )* |
	procedureDeclaration ';' |
	moduleDeclaration ';'
	;

// ***** PIM 4 Appendix 1 lines 79-80 *****

formalParameters :
	'(' ( fpSection ( ';' fpSection )* )? ')' ( ':' qualident )?
	;

// ***** PIM 4 Appendix 1 line 81 *****

fpSection :
	VAR? identList ':' formalType
	;

// ***** PIM 4 Appendix 1 line 82 *****

formalType :
	( ARRAY OF )? qualident
	;

// ***** PIM 4 Appendix 1 lines 83-84 *****

moduleDeclaration :
	MODULE ident priority? ';'
	importList* exportList?
	block ident
	;

// ***** PIM 4 Appendix 1 line 85 *****

priority :
	'[' constExpression ']'
	;

// ***** PIM 4 Appendix 1 line 86 *****

exportList :
	EXPORT QUALIFIED? identList ';'
	;

// ***** PIM 4 Appendix 1 line 87 *****

importList :
	( FROM ident )? IMPORT identList ';'
	;

// ***** PIM 4 Appendix 1 lines 88-89 *****

definitionModule :
	DEFINITION MODULE ident ';'
	importList* exportList? definition*
	END ident '.'
	;

// ***** PIM 4 Appendix 1 lines 90-93 *****

definition :
	CONST ( constantDeclaration ';' )* |
	TYPE ( ident ( '=' type )? ';' )* |
	VAR ( variableDeclaration ';' )* |
	procedureHeading ';'
	;

// ***** PIM 4 Appendix 1 lines 94-95 *****

programModule :
	MODULE ident priority? ';'
	importList* block ident '.'
	;

// ***** PIM 4 Appendix 1 lines 96-97 *****

compilationUnit :	
	definitionModule | IMPLEMENTATION? programModule
	;