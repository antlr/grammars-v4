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
  ;



//Params to mixins, includes, etc

params
  : param (VAR_VALUE_SEPER param)* Ellipsis?
  ;

param
  : ParamName paramOptionalValue?
  ;


variableName
  : DOLLAR Identifier
  ;

paramOptionalValue
  : VAR_VALUE_START paramValue+
  ;


//MIXINS
mixinDeclaration
  : '@mixin' Identifier (ArgumentsStart params? ArgumentsEnd)? block
  ;

//Includes
includeDeclaration
  : INCLUDE Identifier (';' | (ArgumentsStart parameters? ArgumentsEnd ';'?)? block?)
  ;

//FUNCTIONS
functionDeclaration
  : '@function' Identifier ArgumentsStart params? ArgumentsEnd BlockStart functionBody? BlockEnd
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
  : expression (mathCharacter commandStatement)?
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
  | LPAREN conditions RPAREN
  | NULL
  ;

condition
  : commandStatement (COMPARISON conditions)?
  | LPAREN condition RPAREN
  | Identifier
  ;

variableDeclaration
  : variableName COLON value '!default'? ';'
  ;


//for
forDeclaration
  : AT_FOR variableName FROM Number THROUGH Number block
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
	: commandStatement (COMMA? commandStatement)*
	;

url
  : UrlStart Url UrlEnd
  ;

measurement
  : Number Unit?
  ;

interpolation
  : '#' BlockStart variableName BlockEnd
  ;


paramMeasurement
  : ArgumentNumber ArgumentUnit?
  ;

paramUrl
  : UrlArgStart Url UrlEnd
  ;
paramInterpolation
  : InterpolateStart ParamName InterpolateEnd
  ;

paramValue
  : paramMeasurement
  | Expression
  | ParamName
  | paramUrl
  | paramInterpolation
  ;

parameter
  : paramValue (MathChar parameter)?
  | ArgumentsReStart parameter ArgumentsEnd
  ;

parameters
  : parameter (VAR_VALUE_SEPER parameter)*
  ;

functionCall
      //put the multiple params back in
	: Identifier ArgumentsStart parameters? ArgumentsEnd
	;
