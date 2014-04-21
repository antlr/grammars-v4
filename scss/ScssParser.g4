parser grammar ScssParser;

options { tokenVocab=ScssLexer; }

stylesheet
	: statement*
	;

statement
  : importDeclaration
  | nested
  | ruleset
  | mixin
  | functionDeclaration
  | variableDeclaration
  | includeDeclaration
  | ifDeclaration
  | forDeclaration
  | whileDeclaration
  ;



//MIXINS
mixin
  : '@mixin' Identifier (ARGS_START params? ARGS_END)? LBRACE statement* RBRACE
  ;

params
  : paramName paramOptionalValue? (VAR_VALUE_SEPER paramName paramOptionalValue?)* '...'?
  ;

paramName
  : VAR_START VariableName
  ;

variableName
  : DOLLAR Identifier
  ;

paramOptionalValue
  : VAR_VALUE_START Expr+
  ;


//Includes
includeDeclaration
  : INCLUDE Identifier (';' | (ARGS_START includeParams '...'? ARGS_END ';') | '{' statement* '}')
  ;

includeParams
  : includeParam (VAR_VALUE_SEPER includeParam)*
  ;

includeParam
  : Expr+
  | paramName paramOptionalValue?
  ;

//FUNCTIONS
functionDeclaration
  : '@function' Identifier ARGS_START params? ARGS_END LBRACE functionBody? RBRACE
  ;

functionBody
  : functionStatement* '@return' functionStatement
  ;

functionStatement
  : commandStatement ';' | statement
  ;

commandStatement
  : (NUM | variableName) (MATH_CHAR commandStatement)?
  | Identifier LPAREN args RPAREN //function call
  | LPAREN commandStatement RPAREN
  ;




//If statement
ifDeclaration
  : AT_IF conditions block elseIfStatement* elseStatement?
  ;

elseIfStatement
  : AT_ELSE IF conditions block
  ;

elseStatement
  : AT_ELSE block
  ;

conditions
  : condition (COMBINE_COMPARE conditions)?
  | LPAREN conditions RPAREN
  | NULL
  ;

condition
  : (commandStatement | expr) (COMPARISON conditions)?
  | LPAREN condition RPAREN
  | Identifier
  ;

variableDeclaration
  : variableName COLON args ';'
  ;


//for
forDeclaration
  : AT_FOR variableName FROM NUM THROUGH NUM block
  ;


//while
whileDeclaration
  : AT_WHILE variableName conditions block
  ;


//Imports
importDeclaration
	: '@import' referenceUrl mediaTypes? ';'
	;

referenceUrl
    : StringLiteral
    | URL_START referenceUrlValue URL_END
    ;

referenceUrlValue
   : Url
   ;

mediaTypes
  : (Identifier (COMMA Identifier)*)
  ;




//Nested (stylesheets, etc)
nested
 	: '@' nest '{' stylesheet '}'
	;

nest
	: (Identifier | '&') Identifier* pseudo*
	;





//Rules
ruleset
 	: selectors block
	;

block
  :  '{' (statement | properties)* '}'
  ;

selectors
	: selector (COMMA selector)*
	;

selector
	: element+ selectorOperation* attrib* pseudo?
	;

selectorOperation
	: selectop? element
	;

selectop
	: '>'
  | '+'
	;

properties
	: declaration (';' declaration?)*
	;

element
	:     Identifier
	| '#' Identifier
	| '.' Identifier
	;

pseudo
	: (COLON|COLONCOLON) Identifier
	| (COLON|COLONCOLON) function
	;

attrib
	: '[' Identifier (attribRelate (StringLiteral | Identifier))? ']'
	;

attribRelate
	: '='
	| '~='
	| '|='
	;

declaration
	: Identifier COLON args
	;

args
	: expr (COMMA? expr)*
	;

expr
	: NUM UNIT?
	| Identifier
	| COLOR
	| StringLiteral
	| function
	;

	
function
	: Identifier LPAREN args? RPAREN
	;
