/*
 BSD License
 
 Copyright (c) 2022, Tom Everett All rights reserved.
 
 Redistribution and use in source and binary forms, with or without modification, are permitted
 provided that the following conditions are met:
 
 1. Redistributions of source code must retain the above copyright notice, this list of conditions
 and the following disclaimer. 2. Redistributions in binary form must reproduce the above copyright
 notice, this list of conditions and the following disclaimer in the documentation and/or other
 materials provided with the distribution. 3. Neither the name of Tom Everett nor the names of its
 contributors may be used to endorse or promote products derived from this software without specific
 prior written permission.
 
 THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR
 IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND
 FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR
 CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
 DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
 DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER
 IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT
 OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */

// List(elem): elem (, elem)*

grammar croc;

module: moduleDeclaration statement* EOF;

moduleDeclaration:
	decorators? 'module' dottedName statementTerminator;

statementTerminator: ';';

statement:
	assertStatement
	| importStatement
	| blockStatement
	| expressionStatement
	| declarationStatement
	| ifStatement
	| whileStatement
	| doWhileStatement
	| forStatement
	| foreachStatement
	| switchStatement
	| continueStatement
	| breakStatement
	| returnStatement
	| tryCatchStatement
	| throwStatement
	| scopeActionStatement
	| assignment;

dottedName: IDENTIFIER ('.' IDENTIFIER)*;

assertStatement:
	'assert' '(' expression (',' expression)? ')' statementTerminator;

importStatement:
	'import' dottedName ('as' IDENTIFIER)? selectiveImports? statementTerminator;

selectiveImports: ':' list (selectiveImport);

selectiveImport: IDENTIFIER ('as' IDENTIFIER)?;

blockStatement: '{' statement* '}';

expressionStatement: expression statementTerminator;

declarationStatement:
	variableDeclaration statementTerminator
	| decorators? otherDeclaration;

decorators: decorator+;

decorator: '@' dottedName argumentsWith?;

otherDeclaration:
	functionDeclaration
	| classDeclaration
	| namespaceDeclaration;

variableDeclaration: localVarDeclaration | globalVarDeclaration;

localVarDeclaration:
	'local' List (IDENTIFIER) ('=' expressionList)?;

globalVarDeclaration:
	'global' List (IDENTIFIER) ('=' expressionList)?;

functionDeclaration:
	('local' | 'global')? simpleFunctionDeclaration;

simpleFunctionDeclaration: 'function' IDENTIFIER functionBody;

functionBody:
	'(' parameters? ')' returnTypes? (
		blockStatement
		| '=' expression
	);

parameters:
	(parameter | thisParam) (',' parameter)* (',' varargParam)?
	| varargParam;

varargParam: vararg | '...';

thisParam: 'this' ('as' IDENTIFIER)? (':' type | parameter)?;

parameter: IDENTIFIER (':' type)? ('=' Expression)?;

returnTypes:
	':' List (type) (',' '...')?
	| ':' '...'
	| ':' 'void';

type: basicType | 'not' 'null' | 'any' | '@' dottedName;

basicType:
	basicType '|' basicType
	| 'null'
	| 'bool'
	| 'int'
	| 'float'
	| 'string'
	| 'table'
	| 'array'
	| 'memblock'
	| 'function'
	| 'class'
	| 'instance' dottedName?
	| dottedName
	| 'namespace'
	| 'thread'
	| 'nativeobj'
	| 'weakref'
	| 'funcdef';

classDeclaration:
	('local' | 'global')? 'class' IDENTIFIER (
		':' List (expression)
	)? '{' classMember* '}';

classMember:
	decorators? 'override'? simpleFunctionDeclaration
	| decorators? 'override'? 'this' functionBody
	| decorators? 'override'? IDENTIFIER ('=' expression)? statementTerminator;

namespaceDeclaration:
	('local' | 'global') 'namespace' IDENTIFIER (':' expression)? '{' namespaceMember* '}';

namespaceMember:
	decorators? simpleFunctionDeclaration
	| decorators? IDENTIFIER ('=' expression)? statementTerminator;

ifStatement:
	'if' '(' ('local' IDENTIFIER '=')? expression ')' statement (
		'else' statement
	)?;

whileStatement:
	'while' IDENTIFIER? '(' ('local' IDENTIFIER '=')? expression ')' statement;

doWhileStatement:
	'do' statement 'while' IDENTIFIER? '(' expression ')';

forStatement:
	'for' IDENTIFIER? '(' forInitializer? ';' expression? ';' forIncrement? ')' statement
	| 'for' IDENTIFIER? '(' IDENTIFIER ';' expression '..' List (
		expression
	) ')' statement;

forInitializer:
	forInitializer ',' forInitializer
	| expression
	| localVarDeclaration
	| assignment;

forIncrement:
	forIncrement ',' forIncrement
	| expression
	| assignment;

foreachStatement:
	'foreach' IDENTIFIER? '(' List (IDENTIFIER) ';' expression (
		',' expression (',' expression)?
	)? ')' statement;

switchStatement:
	'switch' IDENTIFIER? '(' expression ')' '{' List (
		caseStatement
	) defaultStatement? '}';

caseStatement: 'case' List (expression) ':' statement*;

defaultStatement: 'default' ':' statement*;

continueStatement: 'continue' IDENTIFIER? statementTerminator;

breakStatement: 'break' IDENTIFIER? statementTerminator;

returnStatement:
	'return' List (expression)? statementTerminator;

tryCatchStatement:
	'try' statement catchClauses
	| 'try' statement 'finally' statement
	| 'try' statement catchClauses 'finally' statement;

catchClauses: catchallClause | catchClause+ catchallClause?;

catchClause:
	'catch' '(' IDENTIFIER ':' dottedName ('|' dottedName)* ')' statement;

catchallClause: 'catch' '(' IDENTIFIER ')' statement;

throwStatement: 'throw' expression statementTerminator;

scopeActionStatement:
	'scope' '(' ('exit' | 'success' | 'failure') ')' statement;

assignment:
	List (assignmentLHS) '=' List (expression)
	| assignmentLHS '+=' expression
	| assignmentLHS '-=' expression
	| assignmentLHS '~=' expression
	| assignmentLHS '*=' expression
	| assignmentLHS '/=' expression
	| assignmentLHS '%=' expression
	| assignmentLHS '<<=' expression
	| assignmentLHS '>>=' expression
	| assignmentLHS '>>>=' expression
	| assignmentLHS '|=' expression
	| assignmentLHS '^=' expression
	| assignmentLHS '&=' expression
	| assignmentLHS '?=' expression
	| '++' primaryExpression
	| '--' primaryExpression
	| primaryExpression '++'
	| primaryExpression '--';

assignmentLHS:
	IDENTIFIER
	|
	// Note - for these, the PostfixExpression must start with IDENTIFIER, 'this', ':', or '#'.
	postfixExpression '[' ']'
	| postfixExpression '[' expression? '..' expression? ']'
	| dotExpression
	|
	// In addition to the above, this PostfixExpression may start with 'vararg'
	postfixExpression '[' expression ']';

expression: conditionalExpression;

conditionalExpression:
	logicalConjExpression
	| logicalConjExpression '?' expression ':' conditionalExpression;

logicalConjExpression:
	orExpression
	| orOrExpression
	| andAndExpression;

orOrExpression:
	orOrExpression '||' orExpression
	| orOrExpression 'or' orExpression;

andAndExpression:
	andAndExpression '&&' orExpression
	| andAndExpression 'and' orExpression;

orExpression: xorExpression | orExpression '|' xorExpression;

xorExpression: andExpression | xorExpression '^' andExpression;

andExpression:
	compareExpression
	| andExpression '&' compareExpression;

compareExpression:
	shiftExpression
	| equalExpression
	| relExpression;

equalExpression:
	shiftExpression '==' shiftExpression
	| shiftExpression '!=' shiftExpression
	| shiftExpression 'is' shiftExpression
	| shiftExpression 'is' 'not' shiftExpression;

relExpression:
	shiftExpression '<' shiftExpression
	| shiftExpression '<=' shiftExpression
	| shiftExpression '>' shiftExpression
	| shiftExpression '>=' shiftExpression
	| shiftExpression '<=>' shiftExpression
	| shiftExpression 'in' shiftExpression
	| shiftExpression 'not' 'in' shiftExpression;

shiftExpression:
	addExpression
	| shiftExpression '<<' addExpression
	| shiftExpression '>>' addExpression
	| shiftExpression '>>>' addExpression;

addExpression:
	asExpression
	| addExpression '+' asExpression
	| addExpression '-' asExpression
	| addExpression '~' asExpression;

asExpression: mulExpression | asExpression 'as' asType;

asType: 'bool' | 'int' | 'float' | 'string';

mulExpression:
	unaryExpression
	| mulExpression '*' unaryExpression
	| mulExpression '/' unaryExpression
	| mulExpression '%' unaryExpression;

unaryExpression:
	postfixExpression
	| '-' unaryExpression
	| '!' unaryExpression
	| 'not' unaryExpression
	| '~' unaryExpression
	| '#' unaryExpression;

postfixExpression:
	primaryExpression
	| callExpression
	| postfixExpression '[' ']'
	| postfixExpression '[' expression ']'
	| postfixExpression '[' expression? '..' expression? ']'
	| postfixExpression '.' 'super'
	| dotExpression;

dotExpression:
	postfixExpression '.' (IDENTIFIER | '(' Expression ')');

callExpression:
	postfixExpression argumentsWith
	| dotExpression argumentsWith;

argumentsWith:
	'(' arguments? ')'
	| '(' 'with' expression (',' List (expression))? ')';

arguments: List (expression);

primaryExpression:
	IDENTIFIER
	| 'this'
	| 'null'
	| 'true'
	| 'false'
	| 'vararg'
	| INTLITERAL
	| FLOATLITERAL
	| STRINGLITERAL
	| ':' (IDENTIFIER | 'super' '|' '(' expression ')')
	| 'function' IDENTIFIER? functionBody
	| '\\' parameters (('->' expression) | blockStatement)
	| '(' expression ')'
	| tableCtorExp
	| arrayLiteral
	| 'yield' '(' arguments? ')';

arrayLiteral:
	'[' (expression ','?)* ']'
	| '[' expression forComprehension ']';

tableCtorExp:
	'{' (tableField ','?)* '}'
	| '{' plainTableField forComprehension '}';

tableField:
	IDENTIFIER '=' expression
	| plainTableField
	| simpleFunctionDeclaration;

plainTableField: '[' expression ']' '=' expression;

forComprehension:
	'foreach' List (IDENTIFIER) ';' expression (
		',' expression (',' expression)?
	)? ifComprehension? forComprehension?
	| 'for' IDENTIFIER (':' | ';') expression '..' expression (
		',' expression
	)? ifComprehension? forComprehension?;

ifComprehension: 'if' Expression;

//Comment:
// Character* EndOfLine// NestedComment

//NestedComment:
/* (Character | NestedComment)* */

//LinePragma:
// #line (' ' | \t)+ Decimal ((' ' | \t)+ " Character* ")? EndOfLine

IDENTIFIER: IDENTIFIERSTART IDENTIFIERCHAR*;

IDENTIFIERSTART: '_' | [a .. z]| [A .. Z];

IDENTIFIERCHAR: IDENTIFIERSTART | [0..9]| '!';

STRINGLITERAL: REGULARSTING;
//	|VERBATIMSTRING;

REGULARSTING:
	'"' (Character | ESCAPESEQUENCE |)* '"'
	| '\'' (Character | ESCAPESEQUENCE | EndOfLine)* '\'';
ESCAPESEQUENCE:
	'\''
	| '"'
	| '\\'
	| '\n'
	| '\r'
	| '\t'
	| '\x' HEXDIGIT HEXDIGIT
	| '\u' HEXDIGIT HEXDIGIT HEXDIGIT HEXDIGIT
	| '\U' HEXDIGIT HEXDIGIT HEXDIGIT HEXDIGIT HEXDIGIT HEXDIGIT HEXDIGIT HEXDIGIT
	| '\\' DECIMALDIGIT (DECIMALDIGIT DECIMALDIGIT?)?;

//VERBATIMSTRING:
// @" (Character | EndOfLine)* " | @' (Character | EndOfLine)* ' | [={n}[ (Character | EndOfLine)*
// ]={n}] ;

INTLITERAL: DECIMAL | BINARY | HEXADECIMAL;

DECIMAL: DECIMALDIGIT (DECIMALDIGIT | '_')*;

DECIMALDIGIT: [0 .. 9];

BINARY: '0' ('b' | 'B') (BINARYDIGIT | '_')+;

BINARYDIGIT: [0 .. 1];

HEXADECIMAL: '0' ('x' | 'X') (HEXDIGIT | '_')+;

HEXDIGIT: [0 .. 9] | [a .. f]| [A .. F];

FLOATLITERAL: INTEGER DECIMAL? EXPONENT?;

INTEGER: DECIMALDIGIT (DECIMALDIGIT | '_')*;

DECIMAL: '.' (DECIMALDIGIT | '_')+;

EXPONENT: ('e' | 'E') ('+' | '-')? (DECIMALDIGIT | '_')+;