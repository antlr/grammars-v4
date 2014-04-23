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
  : '@mixin' Identifier (ArgumentsStart params ArgumentsEnd)? block
  ;

//Includes
includeDeclaration
  : INCLUDE Identifier (';' | (ArgumentsStart parameters? ArgumentsEnd ';') | block)
  ;

//FUNCTIONS
functionDeclaration
  : '@function' Identifier ArgumentsStart params? ArgumentsEnd BlockStart functionBody? BlockEnd
  ;

functionBody
  : functionStatement* '@return' functionStatement
  ;

functionStatement
  : commandStatement ';' | statement
  ;

commandStatement
  : expression (MathCharacter commandStatement)?
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
  : commandStatement (COMPARISON conditions)?
  | LPAREN condition RPAREN
  | Identifier
  ;

variableDeclaration
  : variableName COLON value ';'
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
  : BlockStart (property ';' | statement)* property? BlockEnd
  ;

selectors
	: selector (COMMA selector)*
	;

selector
	: element+ selectorOperation* attrib* pseudo?
	;

selectorOperation
	: (GT | PLUS) element
	;



element
	:     Identifier
	| '#' Identifier
	| '.' Identifier
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

property
	: Identifier COLON value
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


expression
  : measurement
  | Identifier
  | Color
  | StringLiteral
  | url
	| variableName
	| functionCall
	;


paramMeasurement
  : ArgumentNumber ArgumentUnit?
  ;

paramUrl
  : UrlArgStart Url UrlEnd
  ;

paramValue
  : paramMeasurement
  | Expression
  | ParamName
  | paramUrl
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
