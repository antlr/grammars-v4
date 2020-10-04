/**
* ObjectiveC version 2
* based on an LL ansic grammars and
* and ObjectiveC grammar found in Learning Object C
*
* It's a Work in progress, most of the .h file can be parsed
* June 2008 Cedric Cuche
**/

grammar ObjectiveC2ansi;
options {
  backtrack=true;
  output=AST;
}

translation_unit: external_declaration+ EOF;

external_declaration:
COMMENT | LINE_COMMENT | preprocessor_declaration
|function_definition
| declaration 
| class_interface
| class_implementation
| category_interface
| category_implementation
| protocol_declaration
| protocol_declaration_list
| class_declaration_list;

preprocessor_declaration:
'#import' file_specification
| '#include' file_specification
| '#define' macro_specification
| '#ifdef' expression
| '#if' expression
| '#undef' expression
| '#ifndef' expression
| '#endif';

file_specification: ('<'|'"')(IDENTIFIER ('/' | '\\' | '.')?)+ ('>' | '"');

macro_specification: '.+';

class_interface:
	'@interface'
	(
	class_name (':' superclass_name)?
	( protocol_reference_list )?
	( instance_variables )?
	( interface_declaration_list )?
	)
	'@end';

category_interface:
	'@interface'
	(
	class_name '(' category_name ')'
	( protocol_reference_list )?
	( interface_declaration_list )?
	)
	'@end';

class_implementation:
	'@implementation'
	(
	class_name ( ':' superclass_name )?
	( implementation_definition_list )?
	)
	'@end';

category_implementation:
	'@implementation'(
	class_name '(' category_name ')'
	( implementation_definition_list )?
	)'@end';

protocol_declaration:
	'@protocol'(
	protocol_name ( protocol_reference_list )?
	( interface_declaration_list )?
	)'@end';

protocol_declaration_list:
	('@protocol' protocol_list';')
	;

class_declaration_list:
	('@class' class_list';')
	;

class_list:
	class_name (',' class_name)*;

protocol_reference_list:
	('<' protocol_list '>');

protocol_list:
	protocol_name (',' protocol_name)*;

class_name:
	IDENTIFIER;

superclass_name:
	IDENTIFIER;

category_name:
	IDENTIFIER;

protocol_name:
	IDENTIFIER;

instance_variables:
	'{' instance_variable_declaration '}';

instance_variable_declaration:	
	(visibility_specification | struct_declarator_list instance_variables)+
	;

visibility_specification:
	'@private'
	| '@protected'
	| '@package' 
	| '@public';

interface_declaration_list:
	(declaration | class_method_declaration | instance_method_declaration)+
	;

class_method_declaration:
	('+' method_declaration)
	;

instance_method_declaration:
	('-' method_declaration)
	;

method_declaration:
	( method_type )? method_selector ';';

implementation_definition_list:
	(function_definition | declaration | class_method_definition | instance_method_definition)+;

class_method_definition:
	('+' method_definition)
	;

instance_method_definition:
	('-' method_definition)
	;
	
method_definition:
	(method_type)? method_selector (init_declarator_list)? compound_statement;

method_selector:
	selector |(keyword_declarator+ (parameter_list)? )
	;

keyword_declarator:
	selector? ':' method_type* IDENTIFIER;

selector:
IDENTIFIER;

method_type:
'(' type_name ')';

type_specifier:
'void' | 'char' | 'short' | 'int' | 'long' | 'float' | 'double' | 'signed' | 'unsigned' 
	|	('id' ( protocol_reference_list )? )
	|	(class_name ( protocol_reference_list )?)
	|	struct_or_union_specifier
	|	enum_specifier 
	|	IDENTIFIER;

type_qualifier:
	'const' | 'volatile' | protocol_qualifier;

protocol_qualifier:
	'in' | 'out' | 'inout' | 'bycopy' | 'byref' | 'oneway';

primary_expression:
	IDENTIFIER
	| constant
	| STRING_LITERAL
	| ('(' expression ')')
	| 'self'
	| message_expression
	| selector_expression
	| protocol_expression
	| encode_expression;

message_expression:
	('[' receiver message_selector ']')
	;

receiver:
	expression
	| class_name 
	| 'super';

message_selector:
	selector
	| keyword_argument+;

keyword_argument:
	selector? ':' expression;

selector_expression:
	'@selector' '(' selector_name ')';

selector_name:
	selector
	| (selector? ':')+;

protocol_expression:
	'@protocol' '(' protocol_name ')';

encode_expression:
	'@encode' '(' type_name ')';

exception_declarator:
	declarator;

try_statement:
	'@trystatement';

catch_statement:
	'@catch' '('exception_declarator')'statement;

finally_statement:
	'@finally' statement;

throw_statement:
	'@throw' '('IDENTIFIER')';

try_block:
	try_statement
	catch_statement
	( finally_statement )?;

synchronized_statement:
	'@synchronized' '(' IDENTIFIER ')' statement;

function_definition : declaration_specifiers declarator compound_statement ;

declaration : declaration_specifiers init_declarator_list? ';';

declaration_specifiers 
  : (storage_class_specifier | type_specifier | type_qualifier)+ ;

storage_class_specifier: 'auto' | 'register' | 'static' | 'extern' | 'typedef';

init_declarator_list :	init_declarator (',' init_declarator)* ;
init_declarator : declarator ('=' initializer)? ;

struct_or_union_specifier: ('struct' | 'union') 
  ( IDENTIFIER | IDENTIFIER? '{' struct_declaration+ '}') ;

struct_declaration : specifier_qualifier_list struct_declarator_list ';' ;

specifier_qualifier_list : (type_specifier | type_qualifier)+ ;

struct_declarator_list : struct_declarator (',' struct_declarator)* ;

struct_declarator : declarator | declarator? ':' constant;

enum_specifier : 'enum' 
  ( identifier ('{' enumerator_list '}')? 
  | '{' enumerator_list '}') ;
enumerator_list : enumerator (',' enumerator)* ;
enumerator : identifier ('=' constant_expression)?;

declarator : '*' type_qualifier* declarator | direct_declarator ;

direct_declarator : identifier declarator_suffix*
                  | '(' declarator ')' declarator_suffix* ;

declarator_suffix : '[' constant_expression? ']'
		  | '(' parameter_list? ')';

parameter_list : parameter_declaration_list ( ',' '...' )? ;

parameter_declaration 
  : declaration_specifiers (declarator? | abstract_declarator) ;

initializer : assignment_expression
	    | '{' initializer (',' initializer)* '}' ;

type_name : specifier_qualifier_list abstract_declarator ;

abstract_declarator : '*' type_qualifier* abstract_declarator 
  | '(' abstract_declarator ')' abstract_declarator_suffix+
  | ('[' constant_expression? ']')+
  | ;

abstract_declarator_suffix
  : '[' constant_expression? ']'
  | '('  parameter_declaration_list? ')' ;

parameter_declaration_list
  : parameter_declaration ( ',' parameter_declaration )* ;

statement_list : (statement)+ ;

statement 
  : labeled_statement
  | expression ';'
  | compound_statement
  | selection_statement
  | iteration_statement
  | jump_statement
  | ';' ;

labeled_statement
  : identifier ':' statement
  | 'case' constant_expression ':' statement
  | 'default' ':' statement ;

compound_statement : '{' (declaration)* statement_list? '}' ;

selection_statement
  : 'if' '(' expression ')' statement ('else' statement)?
  | 'switch' '(' expression ')' statement ;

iteration_statement
  : 'while' '(' expression ')' statement
  | 'do' statement 'while' '(' expression ')' ';'
  | 'for' '(' expression? ';' expression? ';' expression? ')' statement ;

jump_statement
  : 'goto' identifier ';'
  | 'continue' ';'
  | 'break' ';'
  | 'return' expression? ';' 
  ;

expression : assignment_expression (',' assignment_expression)* ;

assignment_expression : conditional_expression 
  ( assignment_operator assignment_expression)? ;
assignment_operator: 
  '=' | '*=' | '/=' | '%=' | '+=' | '-=' | '<<=' | '>>=' | '&=' | '^=' | '|=';

conditional_expression : logical_or_expression 
  ('?' logical_or_expression ':' logical_or_expression)? ;

constant_expression : conditional_expression ;

logical_or_expression :	logical_and_expression 
  ('||' logical_and_expression)* ;

logical_and_expression : inclusive_or_expression 
  ('&&' inclusive_or_expression)* ;

inclusive_or_expression : exclusive_or_expression 
  ('|' exclusive_or_expression)* ;

exclusive_or_expression : and_expression ('^' and_expression)* ;

and_expression : equality_expression ('&' equality_expression)* ;

equality_expression : relational_expression 
  (('!=' | '==') relational_expression)* ;

relational_expression : shift_expression
 (('<' | '>' | '<=' | '>=') shift_expression)* ;

shift_expression : additive_expression (('<<' | '>>') additive_expression)* ;

additive_expression : multiplicative_expression
  (('+' | '-') multiplicative_expression)* ;

multiplicative_expression : cast_expression 
  (('*' | '/' | '%') cast_expression)* ;

cast_expression : '(' type_name ')' cast_expression | unary_expression ;

unary_expression 
  : postfix_expression
  | '++' unary_expression
  | '--' unary_expression
  | unary_operator cast_expression
  | 'sizeof' ('(' type_name ')' | unary_expression) ;

unary_operator : '&' | '*' | '-' | '~' | '!' ;

postfix_expression : primary_expression
  ('[' expression ']' 
  | '(' argument_expression_list? ')'
  | '.' identifier
  | '->' identifier
  | '++'
  | '--'
  )* ;

argument_expression_list
  : assignment_expression (',' assignment_expression)* ;

identifier : IDENTIFIER;

constant : DECIMAL_LITERAL | HEX_LITERAL | OCTAL_LITERAL | CHARACTER_LITERAL | FLOATING_POINT_LITERAL;

IDENTIFIER
	:	LETTER (LETTER|'0'..'9')*
	;
	
fragment
LETTER
	:	'$'
	|	'A'..'Z'
	|	'a'..'z'
	|	'_'
	;

CHARACTER_LITERAL
    :   '\'' ( EscapeSequence | ~('\''|'\\') ) '\''
    ;

STRING_LITERAL
    :  '"' ( EscapeSequence | ~('\\'|'"') )* '"'
    ;

HEX_LITERAL : '0' ('x'|'X') HexDigit+ IntegerTypeSuffix? ;

DECIMAL_LITERAL : ('0' | '1'..'9' '0'..'9'*) IntegerTypeSuffix? ;

OCTAL_LITERAL : '0' ('0'..'7')+ IntegerTypeSuffix? ;

fragment
HexDigit : ('0'..'9'|'a'..'f'|'A'..'F') ;

fragment
IntegerTypeSuffix
	:	('u'|'U'|'l'|'L')
	;

FLOATING_POINT_LITERAL
    :   ('0'..'9')+ ('.' ('0'..'9')*)? Exponent? FloatTypeSuffix?
	;

fragment
Exponent : ('e'|'E') ('+'|'-')? ('0'..'9')+ ;

fragment
FloatTypeSuffix : ('f'|'F'|'d'|'D') ;

fragment
EscapeSequence
    :   '\\' ('b'|'t'|'n'|'f'|'r'|'\"'|'\''|'\\')
    |   OctalEscape
    ;

fragment
OctalEscape
    :   '\\' ('0'..'3') ('0'..'7') ('0'..'7')
    |   '\\' ('0'..'7') ('0'..'7')
    |   '\\' ('0'..'7')
    ;

fragment
UnicodeEscape
    :   '\\' 'u' HexDigit HexDigit HexDigit HexDigit
    ;

WS  :  (' '|'\r'|'\t'|'\u000C'|'\n') {$channel=HIDDEN;}
    ;

COMMENT
    :   '/*' ( options {greedy=false;} : . )* '*/'
    ;

LINE_COMMENT
    : '//' ~('\n'|'\r')* '\r'? '\n'
    ;
