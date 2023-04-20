parser grammar PowerQueryParser;
options {
	tokenVocab = PowerQueryLexer;
}
document: section_document | expression_document;
section_document: section;
section:
	literal_attribs? SECTION section_name SEMICOLON section_members?;
section_name: IDENTIFIER;
section_members: section_member section_members?;
section_member:
	literal_attribs? SHARED? section_member_name EQUALS expression SEMICOLON;
section_member_name: IDENTIFIER;

expression_document: expression;
expression:
	logical_or_expression
	| each_expression
	| function_expression
	| let_expression
	| if_expression
	| let_expression
	| error_raising_expression
	| error_handling_expression;

logical_or_expression:
	logical_and_expression
	| logical_and_expression OR logical_or_expression;
logical_and_expression:
	is_expression
	| logical_and_expression AND is_expression;

is_expression:
	as_expression
	| is_expression IS nullable_primitive_type;
nullable_primitive_type: NULLABLE? primitive_type;

as_expression:
	equality_expression
	| as_expression AS nullable_primitive_type;

equality_expression:
	relational_expression
	| relational_expression EQUALS equality_expression
	| relational_expression NEQ equality_expression;

relational_expression:
	additive_expression
	| additive_expression LE relational_expression
	| additive_expression GE relational_expression
	| additive_expression LEQ additive_expression
	| additive_expression GEQ relational_expression;

additive_expression:
	multiplicative_expression
	| multiplicative_expression PLUS additive_expression
	| multiplicative_expression MINUS additive_expression
	| multiplicative_expression AMP additive_expression;
multiplicative_expression:
	metadata_expression
	| metadata_expression STAR multiplicative_expression
	| metadata_expression SLASH multiplicative_expression;

metadata_expression:
	unary_expression
	| unary_expression META unary_expression;
unary_expression:
	type_expression
	| PLUS unary_expression
	| MINUS unary_expression
	| NOT unary_expression;

primary_expression:
	literal_expression
	| list_expression
	| record_expression
	| identifier_expression
	| section_access_expression
	| parenthesized_expression
	| primary_expression field_selector
	| implicit_target_field_selection
	| primary_expression required_projection
	| primary_expression optional_projection //projection
	| implicit_target_projection //field_access_expression
	| primary_expression OPEN_BRACE item_selector CLOSE_BRACE
	| primary_expression OPEN_BRACE item_selector CLOSE_BRACE OPTIONAL //item access expression 
	| primary_expression OPEN_PAREN argument_list? CLOSE_PAREN //invoke_expression 
	| not_implemented_expression;
literal_expression: LITERAL;
identifier_expression: identifier_reference;
identifier_reference:
	exclusive_identifier_reference
	| inclusive_identifier_reference;
exclusive_identifier_reference: IDENTIFIER;
inclusive_identifier_reference: AT IDENTIFIER;

section_access_expression: IDENTIFIER BANG IDENTIFIER;

parenthesized_expression: OPEN_PAREN expression CLOSE_PAREN;
not_implemented_expression: ELLIPSES;
argument_list: expression | expression COMMA argument_list;
list_expression: OPEN_BRACE item_list? CLOSE_BRACE;
item_list: item | item COMMA item_list;
item: expression | expression DOTDOT expression;

record_expression: OPEN_BRACKET field_list? CLOSE_BRACKET;
field_list: field | field COMMA field_list;
field: field_name EQUALS expression;
field_name: IDENTIFIER;
item_selector: expression;

field_selector:
	required_field_selector
	| optional_field_selector;
required_field_selector: OPEN_BRACKET field_name CLOSE_BRACKET;
optional_field_selector:
	OPEN_BRACKET field_name CLOSE_BRACKET OPTIONAL;
implicit_target_field_selection: field_selector;
required_projection:
	OPEN_BRACKET required_selector_list CLOSE_BRACKET;
optional_projection:
	OPEN_BRACKET required_selector_list CLOSE_BRACKET OPTIONAL;
required_selector_list:
	required_field_selector
	| required_field_selector COMMA required_selector_list;
implicit_target_projection:
	required_projection
	| optional_projection;

function_expression:
	OPEN_PAREN parameter_list? CLOSE_PAREN return_type? '=>' function_body;
function_body: expression;
parameter_list:
	fixed_parameter_list
	| fixed_parameter_list COMMA optional_parameter_list;
fixed_parameter_list:
	parameter
	| parameter COMMA fixed_parameter_list;
parameter: parameter_name parameter_type?;
parameter_name: IDENTIFIER;
parameter_type: assertion;
return_type: assertion;
assertion: AS nullable_primitive_type;
optional_parameter_list:
	optional_parameter
	| optional_parameter COMMA optional_parameter_list;
optional_parameter: OPTIONAL_TEXT parameter;

each_expression: EACH each_expression_body;
each_expression_body: function_body;

let_expression: LET variable_list IN expression;
variable_list: variable | variable COMMA variable_list;
variable: variable_name EQUALS expression;
variable_name: IDENTIFIER;

if_expression:
	IF if_condition THEN true_expression ELSE false_expression;
if_condition: expression;
true_expression: expression;
false_expression: expression;

type_expression: primary_expression | TYPE primary_type;
type_expr: parenthesized_expression | primary_type;
primary_type:
	primitive_type
	| record_type
	| list_type
	| function_type
	| table_type
	| nullable_type;
primitive_type:
	ANY
	| ANYNONNULL
	| BINARY
	| DATE
	| DATETIME
	| DATETIMEZONE
	| DURATION
	| FUNCTION
	| LIST
	| LOGICAL
	| NONE
	| NUMBER
	| RECORD
	| TABLE
	| TEXT
	| TYPE
	| LITERAL;
record_type:
	OPEN_BRACKET open_record_marker CLOSE_BRACKET
	| OPEN_BRACKET field_specification_list? CLOSE_BRACKET
	| OPEN_BRACKET field_specification_list COMMA open_record_marker CLOSE_BRACKET;
field_specification_list:
	field_specification
	| field_specification COMMA field_specification_list;
field_specification:
	OPTIONAL_TEXT? field_name field_type_specification?;
field_type_specification: EQUALS field_type;
field_type: type_expr;
open_record_marker: ELLIPSES;
list_type: OPEN_BRACE item_type CLOSE_BRACE;
item_type: type_expr;
function_type:
	FUNCTION_START parameter_specification_list? CLOSE_PAREN return_type;
parameter_specification_list:
	required_parameter_specification_list
	| required_parameter_specification_list COMMA optional_parameter_specification_list
	| optional_parameter_specification_list;
required_parameter_specification_list:
	required_parameter_specification
	| required_parameter_specification COMMA required_parameter_specification_list;
required_parameter_specification: parameter_specification;
optional_parameter_specification_list:
	optional_parameter_specification
	| optional_parameter_specification COMMA optional_parameter_specification_list;
optional_parameter_specification:
	OPTIONAL_TEXT parameter_specification;
parameter_specification: parameter_name parameter_type;
table_type: TABLE row_type;
row_type: OPEN_BRACKET field_specification_list CLOSE_BRACKET;
nullable_type: NULLABLE type_expr;

error_raising_expression: ERROR expression;
error_handling_expression:
	TRY protected_expression otherwise_clause?;
protected_expression: expression;
otherwise_clause: OTHERWISE default_expression;
default_expression: expression;

literal_attribs: record_literal;
record_literal: OPEN_BRACKET literal_field_list? CLOSE_BRACKET;
literal_field_list:
	literal_field
	| literal_field COMMA literal_field_list;
literal_field: field_name EQUALS any_literal;
list_literal: OPEN_BRACE literal_item_list? CLOSE_BRACE;
literal_item_list:
	any_literal
	| any_literal COMMA literal_item_list;
any_literal: record_literal | list_literal | LITERAL;
