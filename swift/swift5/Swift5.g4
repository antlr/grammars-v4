grammar Swift5;

top_level: statements? EOF;

// Statements
statement:
	(
		expression
		| declaration
		| loop_statement
		| branch_statement
		| labeled_statement
		| control_transfer_statement
		| defer_statement
		| do_statement
	) ';'?
	| compiler_control_statement;

statements: statements_impl[-1];

statements_impl[int indexBefore]
	locals[int indexAfter = -1]:
	{SwiftSupport.isSeparatedStatement(_input, $indexBefore)}? statement {$indexAfter = _input.index();} statements_impl
		[$indexAfter]?;

// Loop Statements
loop_statement: for_in_statement | while_statement | repeat_while_statement;

// For-In Statement
for_in_statement:
	'for' 'case'? pattern 'in' expression where_clause? code_block;

// While Statement
while_statement: 'while' condition_list code_block;

condition_list: condition (',' condition)*;

condition:
	expression
	| availability_condition
	| case_condition
	| optional_binding_condition;

case_condition: 'case' pattern initializer;

optional_binding_condition: ('let' | 'var') pattern initializer;

// Repeat-While Statement
repeat_while_statement: 'repeat' code_block 'while' expression;

// Branch Statements
branch_statement: if_statement | guard_statement | switch_statement;

// If Statement
if_statement: 'if' condition_list code_block else_clause?;
else_clause: 'else' code_block | 'else' if_statement;

// Guard Statement
guard_statement: 'guard' condition_list 'else' code_block;

// Switch Statement
switch_statement: 'switch' expression '{' switch_cases? '}';
switch_cases: switch_case switch_cases?;
switch_case: (case_label | default_label) statements
	| conditional_switch_case;
case_label: attributes? 'case' case_item_list ':';
case_item_list: pattern where_clause? (',' pattern where_clause?)*;
default_label: attributes? 'default' ':';
where_clause: 'where' where_expression;
where_expression: expression;
conditional_switch_case:
	switch_if_directive_clause switch_elseif_directive_clauses? switch_else_directive_clause? endif_directive;
switch_if_directive_clause: if_directive compilation_condition switch_cases?;
switch_elseif_directive_clauses:
	elseif_directive_clause switch_elseif_directive_clauses?;
switch_elseif_directive_clause:
	elseif_directive compilation_condition switch_cases?;
switch_else_directive_clause: else_directive switch_cases?;

// Labeled Statement
labeled_statement:
	statement_label (
		loop_statement
		| if_statement
		| switch_statement
		| do_statement
	);

statement_label: label_name ':';
label_name: identifier;

// Control Transfer Statements
control_transfer_statement:
	break_statement
	| continue_statement
	| fallthrough_statement
	| return_statement
	| throw_statement;

// Break Statement
break_statement: 'break' label_name?;

// Continue Statement
continue_statement: 'continue' label_name?;

// Fallthrough Statement
fallthrough_statement: 'fallthrough';

// Return Statement
return_statement: 'return' expression?;

// Throw Statement
throw_statement: 'throw' expression;

// Defer Statement
defer_statement: 'defer' code_block;

// Do Statement
do_statement: 'do' code_block catch_clauses?;
catch_clauses: catch_clause+;
catch_clause: 'catch' catch_pattern_list? code_block;
catch_pattern_list: catch_pattern (catch_pattern ',' catch_pattern)*;
catch_pattern: pattern where_clause?;

// Compiler Control Statements
compiler_control_statement:
	conditional_compilation_block
	| line_control_statement
	| diagnostic_statement;

// Conditional Compilation Block
conditional_compilation_block:
	if_directive_clause elseif_directive_clauses? else_directive_clause? endif_directive;

if_directive_clause: if_directive compilation_condition statements?;

elseif_directive_clauses: elseif_directive_clause+;
elseif_directive_clause: elseif_directive compilation_condition statements?;
else_directive_clause: else_directive statements?;

if_directive: '#if';
elseif_directive: '#elseif';
else_directive: '#else';
endif_directive: '#endif';

compilation_condition:
	platform_condition
	| identifier
	| boolean_literal
	| '(' compilation_condition ')'
	| '!' compilation_condition
	| compilation_condition (
		compilation_condition_AND
		| compilation_condition_OR
	) compilation_condition;

platform_condition:
	'os' '(' operating_system ')'
	| 'arch' '(' architecture ')'
	| ('swift' | 'compiler') '(' (
		compilation_condition_GE
		| compilation_condition_L
	) swift_version ')'
	| 'canImport' '(' module_name ')'
	| 'targetEnvironment' '(' environment ')';

swift_version: Decimal_digits swift_version_continuation?;
swift_version_continuation: '.' Decimal_digits swift_version_continuation?;

operating_system: 'macOS' | 'iOS' | 'watchOS' | 'tvOS' | 'Linux' | 'Windows';
architecture: 'i386' | 'x86_64' | 'arm' | 'arm64';

module_name: identifier;
environment: 'simulator' | 'macCatalyst';

// Line Control Statement
line_control_statement:
	'#sourceLocation' '(' ('file' ':' file_name ',' 'line' ':' line_number)? ')';

line_number: Decimal_literal; // TODO: A decimal integer greater than zero
file_name: Static_string_literal;

// Compile-Time Diagnostic Statement
diagnostic_statement: ('#error' | '#warning') '(' diagnostic_message ')';

diagnostic_message: Static_string_literal;

// Availability Condition
availability_condition: '#available' '(' availability_arguments ')';

availability_arguments: availability_argument (',' availability_argument)*;

availability_argument: platform_name platform_version | '*';

platform_name:
	'iOS'
	| 'iOSApplicationExtension'
	| 'macOS'
	| 'macOSApplicationExtension'
	| 'macCatalyst'
	| 'macCatalystApplicationExtension'
	| 'watchOS'
	| 'tvOS';

platform_version:
	Decimal_digits
	| Decimal_digits '.' Decimal_digits
	| Decimal_digits '.' Decimal_digits '.' Decimal_digits;

// Generic Parameter Clause
generic_parameter_clause: '<' generic_parameter_list '>';
generic_parameter_list: generic_parameter (',' generic_parameter)*;
generic_parameter:
	type_name (':' (type_identifier | protocol_composition_type))?;

generic_where_clause: 'where' requirement_list;
requirement_list: requirement (',' requirement)*;
requirement: conformance_requirement | same_type_requirement;

conformance_requirement:
	type_identifier ':' (type_identifier | protocol_composition_type);
same_type_requirement:
	type_identifier same_type_equals (type_identifier | type);

// Generic Argument Clause
generic_argument_clause: '<' generic_argument_list '>';
generic_argument_list: generic_argument (',' generic_argument)*;
generic_argument: type;

// Declarations
declaration:
	import_declaration
	| constant_declaration
	| variable_declaration
	| typealias_declaration
	| function_declaration
	| enum_declaration
	| struct_declaration
	| class_declaration
	| protocol_declaration
	| initializer_declaration
	| deinitializer_declaration
	| extension_declaration
	| subscript_declaration
	| operator_declaration
	| precedence_group_declaration;

declarations: declaration+;

// Top-Level Code
top_level_declaration: statements?;

// Code Blocks
code_block: '{' statements? '}';

// Import Declaration
import_declaration: attributes? 'import' import_kind? import_path;
import_kind:
	'typealias'
	| 'struct'
	| 'class'
	| 'enum'
	| 'protocol'
	| 'let'
	| 'var'
	| 'func';
import_path: import_path_identifier ('.' import_path_identifier)*;
import_path_identifier: identifier | operator;

// Constant Declaration
constant_declaration:
	attributes? declaration_modifiers? 'let' pattern_initializer_list;
pattern_initializer_list: pattern_initializer (',' pattern_initializer)*;
pattern_initializer: pattern initializer?;
initializer: '=' expression;

// Variable Declaration
variable_declaration:
	variable_declaration_head (
		pattern_initializer_list
		| variable_name (
			initializer willSet_didSet_block
			| type_annotation (
				getter_setter_block
				| getter_setter_keyword_block
				| initializer? willSet_didSet_block
				| code_block
			)
		)
	);

variable_declaration_head: attributes? declaration_modifiers? 'var';
variable_name: identifier;

getter_setter_block:
	'{' (getter_clause setter_clause? | setter_clause getter_clause) '}'
	| code_block;
getter_clause: attributes? mutation_modifier? 'get' code_block;
setter_clause: attributes? mutation_modifier? 'set' setter_name? code_block;
setter_name: '(' identifier ')';

getter_setter_keyword_block:
	'{' (
		getter_keyword_clause setter_keyword_clause?
		| setter_keyword_clause getter_keyword_clause
	) '}';
getter_keyword_clause: attributes? mutation_modifier? 'get';
setter_keyword_clause: attributes? mutation_modifier? 'set';

willSet_didSet_block:
	'{' (willSet_clause didSet_clause? | didSet_clause willSet_clause?) '}';
willSet_clause: attributes? 'willSet' setter_name? code_block;
didSet_clause: attributes? 'didSet' setter_name? code_block;

// Type Alias Declaration
typealias_declaration:
	attributes? access_level_modifier? 'typealias' typealias_name generic_parameter_clause? typealias_assignment;
typealias_name: identifier;
typealias_assignment: type;

// Function Declaration
function_declaration:
	function_head function_name generic_parameter_clause? function_signature generic_where_clause? function_body?;

function_head: attributes? declaration_modifiers? 'func';

function_name: identifier | operator;

function_signature:
	parameter_clause ('throws'? | 'rethrows') function_result?;

function_result: arrow_operator attributes? type;

function_body: code_block;

parameter_clause: '(' parameter_list? ')';
parameter_list: parameter (',' parameter)*;

parameter:
	external_parameter_name? local_parameter_name type_annotation (
		default_argument_clause?
		| range_operator
	);
external_parameter_name: identifier;
local_parameter_name: identifier;
default_argument_clause: '=' expression;

// Enumeration Declaration
enum_declaration:
	attributes? access_level_modifier? union_style_enum
	| raw_value_style_enum;

union_style_enum:
	'indirect'? 'enum' enum_name generic_parameter_clause? type_inheritance_clause? generic_where_clause? '{'
		union_style_enum_members? '}';

union_style_enum_members: union_style_enum_member+;

union_style_enum_member:
	declaration
	| union_style_enum_case_clause
	| compiler_control_statement;

union_style_enum_case_clause:
	attributes? 'indirect'? 'case' union_style_enum_case_list;

union_style_enum_case_list:
	union_style_enum_case (',' union_style_enum_case)*;

union_style_enum_case:
	opaque_type
	| enum_case_name (tuple_type | '(' type ')')?;

enum_name: identifier;

enum_case_name: identifier;

raw_value_style_enum:
	'enum' enum_name generic_parameter_clause? type_inheritance_clause generic_where_clause? '{'
		raw_value_style_enum_members '}';

raw_value_style_enum_members: raw_value_style_enum_member+;

raw_value_style_enum_member:
	declaration
	| raw_value_style_enum_case_clause
	| compiler_control_statement;

raw_value_style_enum_case_clause:
	attributes? 'case' raw_value_style_enum_case_list;

raw_value_style_enum_case_list:
	raw_value_style_enum_case (',' raw_value_style_enum_case)*;

raw_value_style_enum_case: enum_case_name raw_value_assignment?;

raw_value_assignment: '=' raw_value_literal;

raw_value_literal: numeric_literal | Static_string_literal | boolean_literal;

// Structure Declaration
struct_declaration:
	attributes? access_level_modifier? 'struct' struct_name generic_parameter_clause? type_inheritance_clause?
		generic_where_clause? struct_body;
struct_name: identifier;
struct_body: '{' struct_members? '}';
struct_members: struct_member+;
struct_member: declaration | compiler_control_statement;

// Class Declaration
class_declaration:
	attributes? access_level_modifier? 'final'? 'class' class_name generic_parameter_clause? type_inheritance_clause?
		generic_where_clause? class_body
	| attributes? 'final' access_level_modifier? 'class' class_name generic_parameter_clause? type_inheritance_clause?
		generic_where_clause? class_body;
class_name: identifier;
class_body: '{' class_members '}';
class_members: class_member+;
class_member: declaration | compiler_control_statement;

// Protocol Declaration
protocol_declaration:
	attributes? access_level_modifier? 'protocol' protocol_name type_inheritance_clause? generic_where_clause?
		protocol_body;
protocol_name: identifier;
protocol_body: '{' protocol_members? '}';
protocol_members: protocol_member+;

protocol_member: protocol_member_declaration | compiler_control_statement;

protocol_member_declaration:
	protocol_property_declaration
	| protocol_method_declaration
	| protocol_initializer_declaration
	| protocol_subscript_declaration
	| protocol_associated_type_declaration
	| typealias_declaration;

// Protocol Property Declaration
protocol_property_declaration:
	variable_declaration_head variable_name type_annotation getter_setter_keyword_block;

// Protocol Method Declaration
protocol_method_declaration:
	function_head function_name generic_parameter_clause? function_signature generic_where_clause?;

// Protocol Initializer Declaration
protocol_initializer_declaration:
	initializer_head generic_parameter_clause? parameter_clause (
		'throws'?
		| 'rethrows'
	) generic_where_clause?;

// Protocol Subscript Declaration
protocol_subscript_declaration:
	subscript_head subscript_result generic_where_clause? getter_setter_keyword_block;

// Protocol Associated Type Declaration
protocol_associated_type_declaration:
	attributes? access_level_modifier? 'associatedtype' typealias_name type_inheritance_clause? typealias_assignment?
		generic_where_clause?;

// Initializer Declaration
initializer_declaration:
	initializer_head generic_parameter_clause? parameter_clause (
		'throws'
		| 'rethrows'
	)? generic_where_clause? initializer_body;

initializer_head: attributes? declaration_modifiers? 'init' ('?' | '!')?;

initializer_body: code_block;

// Deinitializer Declaration
deinitializer_declaration: attributes? 'deinit' code_block;

// Extension Declaration
extension_declaration:
	attributes? access_level_modifier? 'extension' type_identifier type_inheritance_clause? generic_where_clause?
		extension_body;
extension_body: '{' extension_members '}';
extension_members: extension_member+;
extension_member: declaration | compiler_control_statement;

// Subscript Declaration
subscript_declaration:
	subscript_head subscript_result generic_where_clause? (
		code_block
		| getter_setter_block
		| getter_setter_keyword_block
	);

subscript_head:
	attributes? declaration_modifiers? 'subscript' generic_parameter_clause? parameter_clause;
subscript_result: arrow_operator attributes? type;

// Operator Declaration
operator_declaration:
	prefix_operator_declaration
	| postfix_operator_declaration
	| infix_operator_declaration;

prefix_operator_declaration: 'prefix' 'operator' operator;
postfix_operator_declaration: 'postfix' 'operator' operator;
infix_operator_declaration: 'infix' 'operator' operator infix_operator_group?;

infix_operator_group: ':' precedence_group_name;

// Precedence Group Declaration
precedence_group_declaration:
	'precedencegroup' precedence_group_name '{' precedence_group_attributes? '}';
precedence_group_attributes: precedence_group_attribute+;

precedence_group_attribute:
	precedence_group_relation
	| precedence_group_assignment
	| precedence_group_associativity;

precedence_group_relation:
	('higherThan' | 'lowerThan') ':' precedence_group_names;

precedence_group_assignment: 'assignment' ':' boolean_literal;

precedence_group_associativity:
	'associativity' ':' ('left' | 'right' | 'none');

precedence_group_names: precedence_group_name (',' precedence_group_name)*;
precedence_group_name: identifier;

// Declaration Modifiers
declaration_modifier:
	'class'
	| 'convenience'
	| 'dynamic'
	| 'final'
	| 'infix'
	| 'lazy'
	| 'optional'
	| 'override'
	| 'postfix'
	| 'prefix'
	| 'required'
	| 'static'
	| 'unowned' ('(' ('safe' | 'unsafe') ')')?
	| 'weak'
	| access_level_modifier
	| mutation_modifier;

declaration_modifiers: declaration_modifier+;

access_level_modifier:
	('private' | 'fileprivate' | 'internal' | 'public' | 'open') (
		'(' 'set' ')'
	)?;

mutation_modifier: 'mutating' | 'nonmutating';

// Patterns

//The following sets of rules are mutually left-recursive [pattern, Type-Casting], to avoid this they were integrated into the same rule.
pattern:
	(wildcard_pattern | identifier_pattern | tuple_pattern) type_annotation?
	| value_binding_pattern
	| enum_case_pattern
	| optional_pattern
	| 'is' type
	| pattern 'as' type
	| expression_pattern;

// Wildcard Pattern
wildcard_pattern: '_';

// identifier Pattern
identifier_pattern: identifier;

// Value-Binding Pattern
value_binding_pattern: 'var' pattern | 'let' pattern;

// Tuple Pattern
tuple_pattern: '(' tuple_pattern_element_list? ')';
tuple_pattern_element_list:
	tuple_pattern_element (',' tuple_pattern_element)*;
tuple_pattern_element: (identifier ':')? pattern;

// Enumeration Case Pattern
enum_case_pattern: type_identifier? '.' enum_case_name tuple_pattern?;

// Optional Pattern
optional_pattern: identifier_pattern '?';

// Expression Pattern
expression_pattern: expression;

// Attributes
attribute: '@' attribute_name attribute_argument_clause?;
attribute_name: identifier;
attribute_argument_clause: '(' balanced_tokens? ')';
attributes: attribute+;
balanced_tokens: balanced_token+;

balanced_token:
	'(' balanced_tokens? ')'
	| '[' balanced_tokens? ']'
	| '{' balanced_tokens? '}'
	//Any identifier, keyword, literal, or operator Any punctuation except (, ), [, ], {, or }
	| identifier
	| Keyword
	| literal
	| operator
	| balanced_token_punctuation;

balanced_token_punctuation:
	('.' | ',' | ':' | ';' | '=' | '@' | '#' | '`' | '?')
	| arrow_operator
	| {SwiftSupport.isPrefixOp(_input)}? '&'
	| {SwiftSupport.isPostfixOp(_input)}? '!';

// Expressions
expression: try_operator? prefix_expression binary_expressions?;

expression_list: expression (',' expression)*;

// Prefix Expressions
prefix_expression: prefix_operator? postfix_expression | in_out_expression;

in_out_expression: '&' identifier;

// Try Operator
try_operator: 'try' '?' | 'try' '!' | 'try';

// Binary Expressions
binary_expression:
	binary_operator prefix_expression
	| (assignment_operator | conditional_operator) try_operator? prefix_expression
	| type_casting_operator;

binary_expressions: binary_expression+;

// Conditional Operator
conditional_operator: '?' expression ':';

// Ternary Conditional Operator
type_casting_operator: ('is' | 'as' ( '?' | '!')?) type;

// Primary Expressions
primary_expression:
	identifier generic_argument_clause?
	| literal_expression
	| self_expression
	| superclass_expression
	| closure_expression
	| parenthesized_expression
	| tuple_expression
	| implicit_member_expression
	| wildcard_expression
	| key_path_expression
	| selector_expression
	| key_path_string_expression;

// Literal Expression
literal_expression:
	literal
	| array_literal
	| dictionary_literal
	| playground_literal
	| '#file'
	| '#fileID'
	| '#filePath'
	| '#line'
	| '#column'
	| '#function'
	| '#dsohandle';

array_literal: '[' array_literal_items? ']';

array_literal_items: array_literal_item (',' array_literal_item)* ','?;

array_literal_item: expression;

dictionary_literal: '[' (dictionary_literal_items | ':') ']';

dictionary_literal_items:
	dictionary_literal_item (',' dictionary_literal_item)* ','?;

dictionary_literal_item: expression ':' expression;

playground_literal:
	'#colorLiteral' '(' 'red' ':' expression ',' 'green' ':' expression ',' 'blue' ':' expression ',' 'alpha' ':'
		expression ')'
	| '#fileLiteral' '(' 'resourceName' ':' expression ')'
	| '#imageLiteral' '(' 'resourceName' ':' expression ')';

// Self Expression
self_expression:
	'self'
	| self_method_expression
	| self_subscript_expression
	| self_initializer_expression;

self_method_expression: 'self' '.' identifier;

self_subscript_expression: 'self' '[' function_call_argument_list ']';

self_initializer_expression: 'self' '.' 'init';

// Superclass Expression
superclass_expression:
	superclass_method_expression
	| superclass_subscript_expression
	| superclass_initializer_expression;

superclass_method_expression: 'super' '.' identifier;
superclass_subscript_expression: 'super' '[' function_call_argument_list ']';
superclass_initializer_expression: 'super' '.' 'init';

// Capture Lists
closure_expression: '{' closure_signature? statements? '}';

closure_signature:
	capture_list? closure_parameter_clause 'throws'? function_result? 'in'
	| capture_list 'in';

closure_parameter_clause: '(' closure_parameter_list? ')' | identifier_list;

closure_parameter_list: closure_parameter (',' closure_parameter)*;

closure_parameter: closure_parameter_name (type_annotation range_operator?)?;

closure_parameter_name: identifier;

capture_list: '[' capture_list_items ']';

capture_list_items: capture_list_item (',' capture_list_item)*;

capture_list_item: capture_specifier? expression;

capture_specifier: 'weak' | 'unowned' | 'unowned(safe)' | 'unowned(unsafe)';

// Implicit Member Expression
implicit_member_expression:
	'.' identifier; // let a: MyType = .default; static let `default` = MyType()

// Parenthesized Expression
parenthesized_expression: '(' expression ')';

// Tuple Expression
tuple_expression: '(' ')' | '(' tuple_element ',' tuple_element_list ')';

tuple_element_list: tuple_element (',' tuple_element)*;

tuple_element: expression | identifier ':' expression;

// Wildcard Expression
wildcard_expression: '_';

// Key-Path Expression
key_path_expression: '\\' type? '.' key_path_components;
key_path_components: key_path_component ('.' key_path_component)*;
key_path_component: identifier key_path_postfixes? | key_path_postfixes;
key_path_postfixes: key_path_postfix+;
key_path_postfix: '?' | '!' | 'self' | '[' function_call_argument_list ']';

// Selector Expression
selector_expression: '#selector' '(' ('getter:' | 'setter:')? expression ')';

// Key-Path String Expression
key_path_string_expression: '#keyPath' '(' expression ')';

// Postfix Expressions

//The following sets of rules are mutually left-recursive [postfix_expression, function_call_expression,
// initializer_expression, explicit_member_expression, postfix_self_expression, subscript_expression,
// forced_value_expression, optional_chaining_expression], to avoid this the rule inner_postfix_expression was
// implemented.
postfix_expression:
	postfix_expression postfix_operator
	| function_call_expression
	| initializer_expression
	| explicit_member_expression
	| postfix_self_expression
	| subscript_expression
	| forced_value_expression
	| optional_chaining_expression
	| primary_expression;

inner_postfix_expression:
	function_call_argument_clause
	| function_call_argument_clause? trailing_closures
	| '.' (
		'init' ('(' argument_names ')')?
		| Decimal_digits
		| identifier (generic_argument_clause | '(' argument_names ')')?
		| 'self'
	)
	| '[' function_call_argument_list ']'
	| '!'
	| '?';

// Function Call Expression
function_call_expression:
	primary_expression inner_postfix_expression* (
		function_call_argument_clause
		| function_call_argument_clause? trailing_closures
	);

function_call_argument_clause: '(' function_call_argument_list? ')';

function_call_argument_list:
	function_call_argument (',' function_call_argument)*;

function_call_argument:
	expression
	| identifier ':' (expression | operator)
	| operator;

trailing_closures: closure_expression labeled_trailing_closures?;

labeled_trailing_closures: labeled_trailing_closure+;

labeled_trailing_closure: identifier ':' closure_expression;

// Initializer Expression
initializer_expression:
	primary_expression inner_postfix_expression* '.' 'init' (
		'(' argument_names ')'
	)?;

// Explicit Member Expression
explicit_member_expression:
	primary_expression inner_postfix_expression* '.' (
		Decimal_digits
		| identifier (generic_argument_clause | '(' argument_names ')')?
	);

argument_names: argument_name+;
argument_name: identifier ':';

// Postfix Self Expression
postfix_self_expression:
	primary_expression inner_postfix_expression* '.' 'self';

// Subscript Expression
subscript_expression:
	primary_expression inner_postfix_expression* '[' function_call_argument_list ']';

// Forced-Value Expression
forced_value_expression: primary_expression inner_postfix_expression* '!';

// Optional-Chaining Expression
optional_chaining_expression:
	primary_expression inner_postfix_expression* '?';

// Types

// The following sets of rules are mutually left-recursive [type, optional_type, implicitly_unwrapped_optional_type, metatype_type], to avoid this they were integrated into the same rule.
type:
	function_type
	| array_type
	| dictionary_type
	| type_identifier
	| tuple_type
	| protocol_composition_type
	| opaque_type
	| type (
		'?' //optional_type
		| '!' //implicitly_unwrapped_optional_type
		| '.' 'Type'
		| '.' 'Protocol'
	) //metatype_type
	| any_type
	| self_type
	| '(' type ')';

// Type Annotation
type_annotation: ':' attributes? 'inout'? type;

// Type identifier
type_identifier: type_name generic_argument_clause? ('.' type_identifier)?;

type_name: identifier;

// Tuple Type
tuple_type: '(' tuple_type_element_list? ')';
tuple_type_element_list: tuple_type_element (',' tuple_type_element)+;
tuple_type_element: element_name type_annotation | type;
element_name: identifier;

// Function Type
function_type:
	attributes? function_type_argument_clause 'throws'? arrow_operator type;

function_type_argument_clause:
	'(' (function_type_argument_list range_operator?)? ')';

function_type_argument_list:
	function_type_argument (',' function_type_argument)*;

function_type_argument:
	attributes? 'inout'? type
	| argument_label type_annotation;

argument_label: identifier;

// Array Type
array_type: '[' type ']';

// Dictionary Type
dictionary_type: '[' type ':' type ']';

// Protocol Composition Type
protocol_composition_type:
	type_identifier '&' protocol_composition_continuation;
protocol_composition_continuation:
	type_identifier
	| protocol_composition_type;

// Opaque Type
opaque_type: 'some' type;

// Any Type
any_type: 'Any';

// Self Type
self_type: 'Self';

// Type Inheritance Clause
type_inheritance_clause: ':' type_inheritance_list;

type_inheritance_list: type_identifier (',' type_identifier)*;

// Identifiers
identifier:
	(
		'Linux'
		| 'Windows'
		| 'alpha'
		| 'arch'
		| 'arm'
		| 'arm64'
		| 'assignment'
		| 'blue'
		| 'canImport'
		| 'compiler'
		| 'file'
		| 'green'
		| 'higherThan'
		| 'i386'
		| 'iOS'
		| 'iOSApplicationExtension'
		| 'line'
		| 'lowerThan'
		| 'macCatalyst'
		| 'macCatalystApplicationExtension'
		| 'macOS'
		| 'macOSApplicationExtension'
		| 'os'
		| 'precedencegroup'
		| 'red'
		| 'resourceName'
		| 'safe'
		| 'simulator'
		| 'some'
		| 'swift'
		| 'targetEnvironment'
		| 'tvOS'
		| 'u'
		| 'unsafe'
		| 'watchOS'
		| 'x86_64'

		// Keywords reserved in particular contexts
		| 'associativity'
		| 'convenience'
		| 'dynamic'
		| 'didSet'
		| 'final'
		| 'get'
		| 'infix'
		| 'indirect'
		| 'lazy'
		| 'left'
		| 'mutating'
		| 'none'
		| 'nonmutating'
		| 'optional'
		| 'override'
		| 'postfix'
		| 'precedence'
		| 'prefix'
		| 'Protocol'
		| 'required'
		| 'right'
		| 'set'
		| 'Type'
		| 'unowned'
		| 'weak'
		| 'willSet'
	)
	| Identifier;

Identifier:
	Identifier_head Identifier_characters?
	| '`' (Keyword | Identifier_head Identifier_characters?) '`'
	| Implicit_parameter_name
	| Property_wrapper_projection;

identifier_list: identifier | (',' identifier);

fragment Identifier_head:
	[a-zA-Z]
	| '_'
	| '\u00A8'
	| '\u00AA'
	| '\u00AD'
	| '\u00AF'
	| [\u00B2-\u00B5]
	| [\u00B7-\u00BA]
	| [\u00BC-\u00BE]
	| [\u00C0-\u00D6]
	| [\u00D8-\u00F6]
	| [\u00F8-\u00FF]
	| [\u0100-\u02FF]
	| [\u0370-\u167F]
	| [\u1681-\u180D]
	| [\u180F-\u1DBF]
	| [\u1E00-\u1FFF]
	| [\u200B-\u200D]
	| [\u202A-\u202E]
	| [\u203F-\u2040]
	| '\u2054'
	| [\u2060-\u206F]
	| [\u2070-\u20CF]
	| [\u2100-\u218F]
	| [\u2460-\u24FF]
	| [\u2776-\u2793]
	| [\u2C00-\u2DFF]
	| [\u2E80-\u2FFF]
	| [\u3004-\u3007]
	| [\u3021-\u302F]
	| [\u3031-\u303F]
	| [\u3040-\uD7FF]
	| [\uF900-\uFD3D]
	| [\uFD40-\uFDCF]
	| [\uFDF0-\uFE1F]
	| [\uFE30-\uFE44]
	| [\uFE47-\uFFFD]
	| [\u{10000}-\u{1FFFD}]
	| [\u{20000}-\u{2FFFD}]
	| [\u{30000}-\u{3FFFD}]
	| [\u{40000}-\u{4FFFD}]
	| [\u{50000}-\u{5FFFD}]
	| [\u{60000}-\u{6FFFD}]
	| [\u{70000}-\u{7FFFD}]
	| [\u{80000}-\u{8FFFD}]
	| [\u{90000}-\u{9FFFD}]
	| [\u{A0000}-\u{AFFFD}]
	| [\u{B0000}-\u{BFFFD}]
	| [\u{C0000}-\u{CFFFD}]
	| [\u{D0000}-\u{DFFFD}]
	| [\u{E0000}-\u{EFFFD}];

fragment Identifier_character:
	[0-9]
	| [\u0300-\u036F]
	| [\u1DC0-\u1DFF]
	| [\u20D0-\u20FF]
	| [\uFE20-\uFE2F]
	| Identifier_head;

fragment Identifier_characters: Identifier_character+;

fragment Implicit_parameter_name: '$' Decimal_digits;

fragment Property_wrapper_projection: '$' Identifier_characters;

// Keywords and Punctuation
Keyword:
	// Keywords used in declarations
	'associatedtype'
	| 'class'
	| 'deinit'
	| 'enum'
	| 'extension'
	| 'fileprivate'
	| 'func'
	| 'import'
	| 'init'
	| 'inout'
	| 'internal'
	| 'let'
	| 'open'
	| 'operator'
	| 'private'
	| 'protocol'
	| 'public'
	| 'rethrows'
	| 'static'
	| 'struct'
	| 'subscript'
	| 'typealias'
	| 'var'
	// Keywords used in statements
	| 'break'
	| 'case'
	| 'continue'
	| 'default'
	| 'defer'
	| 'do'
	| 'else'
	| 'fallthrough'
	| 'for'
	| 'guard'
	| 'if'
	| 'in'
	| 'repeat'
	| 'return'
	| 'switch'
	| 'where'
	| 'while'
	// Keywords used in expressions and types
	| 'as'
	| 'Any'
	| 'catch'
	| 'false'
	| 'is'
	| 'nil'
	| 'super'
	| 'self'
	| 'Self'
	| 'throw'
	| 'throws'
	| 'true'
	| 'try'
	// Keywords used in patterns
	| UNDERSCORE
	// Keywords that begin with a number sign (#)
	| '#available'
	| '#colorLiteral'
	| '#column'
	| '#else'
	| '#elseif'
	| '#endif'
	| '#error'
	| '#file'
	| '#fileID'
	| '#fileLiteral'
	| '#filePath'
	| '#function'
	| '#if'
	| '#imageLiteral'
	| '#line'
	| '#selector'
	| '#sourceLocation'
	| '#warning';

// Operators

// Assignment Operator
assignment_operator: {SwiftSupport.isBinaryOp(_input)}? '=';

DOT: '.';
LCURLY: '{';
LPAREN: '(';
LBRACK: '[';
RCURLY: '}';
RPAREN: ')';
RBRACK: ']';
COMMA: ',';
COLON: ':';
SEMI: ';';
LT: '<';
GT: '>';
UNDERSCORE: '_';
BANG: '!';
QUESTION: '?';
AT: '@';
AND: '&';
SUB: '-';
EQUAL: '=';
OR: '|';
DIV: '/';
ADD: '+';
MUL: '*';
MOD: '%';
CARET: '^';
TILDE: '~';

negate_prefix_operator: {SwiftSupport.isPrefixOp(_input)}? '-';

compilation_condition_AND: {SwiftSupport.isOperator(_input,"&&")}? '&' '&';
compilation_condition_OR: {SwiftSupport.isOperator(_input,"||")}? '|' '|';
compilation_condition_GE: {SwiftSupport.isOperator(_input,">=")}? '>' '=';
compilation_condition_L: {SwiftSupport.isOperator(_input,"<")}? '<';
arrow_operator: {SwiftSupport.isOperator(_input,"->")}? '-' '>';
range_operator: {SwiftSupport.isOperator(_input,"...")}? '.' '.' '.';
same_type_equals: {SwiftSupport.isOperator(_input,"==")}? '=' '=';

binary_operator: {SwiftSupport.isBinaryOp(_input)}? operator;

prefix_operator: {SwiftSupport.isPrefixOp(_input)}? operator;

postfix_operator: {SwiftSupport.isPostfixOp(_input)}? operator;

operator:
	operator_head operator_characters?
	| dot_operator_head dot_operator_characters;

operator_head: (
		'/'
		| '='
		| '-'
		| '+'
		| '!'
		| '*'
		| '%'
		| '&'
		| '|'
		| '<'
		| '>'
		| '^'
		| '~'
		| '?'
	) // wrapping in (..) makes it a fast set comparison
	| Operator_head_other;

Operator_head_other: // valid operator chars not used by Swift itself
	[\u00A1-\u00A7]
	| [\u00A9\u00AB]
	| [\u00AC\u00AE]
	| [\u00B0-\u00B1\u00B6\u00BB\u00BF\u00D7\u00F7]
	| [\u2016-\u2017\u2020-\u2027]
	| [\u2030-\u203E]
	| [\u2041-\u2053]
	| [\u2055-\u205E]
	| [\u2190-\u23FF]
	| [\u2500-\u2775]
	| [\u2794-\u2BFF]
	| [\u2E00-\u2E7F]
	| [\u3001-\u3003]
	| [\u3008-\u3020\u3030];

operator_character: operator_head | Operator_following_character;
Operator_following_character:
	[\u0300-\u036F]
	| [\u1DC0-\u1DFF]
	| [\u20D0-\u20FF]
	| [\uFE00-\uFE0F]
	| [\uFE20-\uFE2F]
	| [\u{E0100}-\u{E01EF}];

operator_characters: (
		{_input.get(_input.index()-1).getType()!=WS}? operator_character
	)+;

dot_operator_head: '.';
dot_operator_character: '.' | operator_character;
dot_operator_characters: (
		{_input.get(_input.index()-1).getType()!=WS}? dot_operator_character
	)+;

// Literals
literal: numeric_literal | string_literal | boolean_literal | nil_literal;

numeric_literal:
	negate_prefix_operator? integer_literal
	| negate_prefix_operator? Floating_point_literal;

boolean_literal: 'true' | 'false';

nil_literal: 'nil';

// Integer Literals
integer_literal:
	Decimal_digits
	| Decimal_literal
	| Binary_literal
	| Octal_literal
	| Hexadecimal_literal;

Binary_literal: '0b' Binary_digit Binary_literal_characters?;
fragment Binary_digit: [01];
fragment Binary_literal_character: Binary_digit | '_';
fragment Binary_literal_characters: Binary_literal_character+;

Octal_literal: '0o' Octal_digit Octal_literal_characters?;
fragment Octal_digit: [0-7];
fragment Octal_literal_character: Octal_digit | '_';
fragment Octal_literal_characters: Octal_literal_character+;

Decimal_digits: Decimal_digit+;
Decimal_literal: Decimal_digit Decimal_literal_characters?;
fragment Decimal_digit: [0-9];
fragment Decimal_literal_character: Decimal_digit | '_';
fragment Decimal_literal_characters: Decimal_literal_character+;

Hexadecimal_literal: '0x' Hexadecimal_digit Hexadecimal_literal_characters?;
fragment Hexadecimal_digit: [0-9a-fA-F];
fragment Hexadecimal_literal_character: Hexadecimal_digit | '_';
fragment Hexadecimal_literal_characters: Hexadecimal_literal_character+;

// Floating-Point Literals
Floating_point_literal:
	Decimal_literal Decimal_fraction? Decimal_exponent?
	| Hexadecimal_literal Hexadecimal_fraction? Hexadecimal_exponent;
fragment Decimal_fraction: '.' Decimal_literal;
fragment Decimal_exponent: Floating_point_e Sign? Decimal_literal;
fragment Hexadecimal_fraction:
	'.' Hexadecimal_digit Hexadecimal_literal_characters?;
fragment Hexadecimal_exponent: Floating_point_p Sign? Decimal_literal;
fragment Floating_point_e: [eE];
fragment Floating_point_p: [pP];
fragment Sign: [+-];

// String Literals
string_literal:
	Extended_string_literal_delimiter string_literal Extended_string_literal_delimiter
	| Static_string_literal
	| Interpolated_string_literal;

Static_string_literal:
	'"' Quoted_text? '"'
	| '"""' [\r\n] Multiline_quoted_text? [\r\n] '"""';

Extended_string_literal_delimiter: '#';

fragment Quoted_text: Quoted_text_item+;
fragment Quoted_text_item: Escaped_character | ~["\n\r\\];

fragment Multiline_quoted_text: Multiline_quoted_text_item+?;

fragment Multiline_quoted_text_item:
	Escaped_character
	| ~[\\]
	| Escaped_newline;

Interpolated_string_literal:
	'"' Interpolated_text? '"'
	| '"""' [\r\n] Multiline_interpolated_text? [\r\n] '"""';

fragment Interpolated_text: Interpolated_text_item+;

fragment Interpolated_text_item:
	'\\(' (Interpolated_string_literal | Interpolated_text_item)+ ')'
	| Quoted_text_item;

fragment Multiline_interpolated_text: Multiline_interpolated_text_item+;

fragment Multiline_interpolated_text_item:
	'\\(' (Interpolated_string_literal | Multiline_interpolated_text_item)+ ')'
	| Multiline_quoted_text_item;

fragment Escape_sequence: '\\' Extended_string_literal_delimiter;

fragment Escaped_character:
	Escape_sequence ([0\\tnr"'] | 'u' '{' Unicode_scalar_digits '}');

fragment Unicode_scalar_digits: Hexadecimal_literal+;
//Between one and eight hexadecimal digits

fragment Escaped_newline: Escape_sequence Inline_spaces? Line_break;

fragment Inline_spaces: [\u0009\u0020];

fragment Line_break: [\u000A\u000D]| '\u000D' '\u000A';

WS: [ \n\r\t\u000B\u000C\u0000]+ -> channel(HIDDEN);

Block_comment: '/*' (Block_comment | .)*? '*/' -> channel(HIDDEN);

Line_comment: '//' .*? ('\n' | EOF) -> channel(HIDDEN);