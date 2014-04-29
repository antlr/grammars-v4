parser grammar ScssParser;

options { tokenVocab=ScssLexer; }

stylesheet
	: statement*
	;

statement
  : importDeclaration
  | nested
  | ruleset
  | mixinDeclaration
  | functionDeclaration
  | variableDeclaration
  | includeDeclaration
  | ifDeclaration
  | forDeclaration
  | whileDeclaration
  | eachDeclaration
  ;



//Params to mixins, includes, etc
params
  : param (COMMA param)* Ellipsis?
  ;

param
  : variableName paramOptionalValue?
  ;

variableName
  : DOLLAR Identifier
  ;

paramOptionalValue
  : COLON expression+
  ;


//MIXINS
mixinDeclaration
  : '@mixin' Identifier (LPAREN params? RPAREN)? block
  ;

//Includes
includeDeclaration
  : INCLUDE Identifier (';' | (LPAREN value? RPAREN ';'?)? block?)
  ;

//FUNCTIONS
functionDeclaration
  : '@function' Identifier LPAREN params? RPAREN BlockStart functionBody? BlockEnd
  ;

functionBody
  : functionStatement* functionReturn
  ;

functionReturn
  : '@return' commandStatement ';'
  ;

functionStatement
  : commandStatement ';' | statement
  ;

mathCharacter
  : TIMES | PLUS | '/' | '-' | '%'
  ;

commandStatement
  : expression+ (mathCharacter commandStatement)?
  | LPAREN commandStatement RPAREN
  ;


expression
  : measurement
  | identifier
  | Color
  | StringLiteral
  | url
	| variableName
	| functionCall
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
  | NULL
  ;

condition
  : commandStatement (( '==' | LT | GT | '!=') conditions)?
  | LPAREN conditions ')'
  ;

variableDeclaration
  : variableName COLON value '!default'? ';'
  ;


//for
forDeclaration
  : AT_FOR variableName 'from' fromNumber 'through' throughNumber block
  ;

fromNumber
  : Number
  ;
throughNumber
  : Number
  ;

//while
whileDeclaration
  : AT_WHILE conditions block
  ;

//EACH
eachDeclaration
  : AT_EACH variableName (COMMA variableName)* IN eachValueList block
  ;

eachValueList
  :  Identifier (COMMA Identifier)*
  |  identifierListOrMap (COMMA identifierListOrMap)*
  ;

identifierListOrMap
  : LPAREN identifierValue (COMMA identifierValue)* RPAREN
  ;
identifierValue
  : identifier (COLON value)?
  ;


//Imports
importDeclaration
	: '@import' referenceUrl mediaTypes? ';'
	;

referenceUrl
    : StringLiteral
    | UrlStart Url UrlEnd
    ;


mediaTypes
  : (Identifier (COMMA Identifier)*)
  ;




//Nested (stylesheets, etc)
nested
 	: '@' nest BlockStart stylesheet BlockEnd
	;

nest
	: (Identifier | '&') Identifier* pseudo*
	;





//Rules
ruleset
 	: selectors block
	;

block
  : BlockStart (property ';' | statement)* property? BlockEnd
  ;

selectors
	: selector (COMMA selector)*
	;

selector
	: element (selectorPrefix? element)* attrib* pseudo?
	;

selectorPrefix
  : (GT | PLUS | TIL | SPACE)
  ;

element
	: identifier
  | '#' identifier
  | '.' identifier
  | '&'
  | TIMES
	;

pseudo
	: (COLON|COLONCOLON) Identifier
	| (COLON|COLONCOLON) functionCall
	;

attrib
	: '[' Identifier (attribRelate (StringLiteral | Identifier))? ']'
	;

attribRelate
	: '='
	| '~='
	| '|='
	;

identifier
  : (Identifier | interpolation)+
  ;

property
	: identifier COLON value
	;

value
	: commandStatement (COMMA commandStatement)*
	;

url
  : UrlStart Url UrlEnd
  ;

measurement
  : Number Unit?
  ;

interpolation
  : HASH BlockStart variableName BlockEnd
  ;



paramUrl
  : UrlArgStart Url UrlEnd
  ;


functionCall
      //put the multiple params back in
	: Identifier LPAREN value? RPAREN
	;
