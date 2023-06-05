// Eclipse Public License - v 1.0, http://www.eclipse.org/legal/epl-v10.html
// Copyright (c) 2013, Christian Wulf (chwchw@gmx.de)
// Copyright (c) 2016-2017, Ivan Kochurkin (kvanttt@gmail.com), Positive Technologies.

parser grammar CSharpParser;

options { tokenVocab=CSharpLexer; superClass = CSharpParserBase; }

// entry point
compilation_unit
	: BYTE_ORDER_MARK? extern_alias_directives? using_directives?
	  global_attribute_section* namespace_member_declarations? EOF
	;

//B.2 Syntactic grammar

//B.2.1 Basic concepts

namespace_or_type_name
	: (identifier type_argument_list? | qualified_alias_member) ('.' identifier type_argument_list?)*
	;

//B.2.2 Types
type_
	: base_type ('?' | rank_specifier | '*')*
	;

base_type
	: simple_type
	| class_type  // represents types: enum, class, interface, delegate, type_parameter
	| VOID '*'
	| tuple_type
	;

tuple_type
    : '(' tuple_element (',' tuple_element)+ ')'
    ;

tuple_element
    : type_ identifier?
    ;

simple_type
	: numeric_type
	| BOOL
	;

numeric_type
	: integral_type
	| floating_point_type
	| DECIMAL
	;

integral_type
	: SBYTE
	| BYTE
	| SHORT
	| USHORT
	| INT
	| UINT
	| LONG
	| ULONG
	| CHAR
	;

floating_point_type
	: FLOAT
	| DOUBLE
	;

/** namespace_or_type_name, OBJECT, STRING */
class_type
	: namespace_or_type_name
	| OBJECT
	| DYNAMIC
	| STRING
	;

type_argument_list
	: '<' type_ ( ',' type_)* '>'
	;

//B.2.4 Expressions
argument_list
	: argument ( ',' argument)*
	;

argument
	: (identifier ':')? refout=(REF | OUT | IN)?
	  ( expression
	  | (VAR | type_) expression
	  )
	;

expression
	: assignment
	| non_assignment_expression
	| REF non_assignment_expression
	;

non_assignment_expression
	: lambda_expression
	| query_expression
	| conditional_expression
	;

assignment
	: unary_expression assignment_operator expression
	| unary_expression '??=' throwable_expression
	;

assignment_operator
	: '=' | '+=' | '-=' | '*=' | '/=' | '%=' | '&=' | '|=' | '^=' | '<<=' | right_shift_assignment
	;

conditional_expression
	: null_coalescing_expression ('?' throwable_expression ':' throwable_expression)?
	;

null_coalescing_expression
	: conditional_or_expression ('??' (null_coalescing_expression | throw_expression))?
	;

conditional_or_expression
	: conditional_and_expression (OP_OR conditional_and_expression)*
	;

conditional_and_expression
	: inclusive_or_expression (OP_AND inclusive_or_expression)*
	;

inclusive_or_expression
	: exclusive_or_expression ('|' exclusive_or_expression)*
	;

exclusive_or_expression
	: and_expression ('^' and_expression)*
	;

and_expression
	: equality_expression ('&' equality_expression)*
	;

equality_expression
	: relational_expression ((OP_EQ | OP_NE)  relational_expression)*
	;

relational_expression
	: shift_expression (('<' | '>' | '<=' | '>=') shift_expression | IS isType | AS type_)*
	;

shift_expression
	: additive_expression (('<<' | right_shift)  additive_expression)*
	;

additive_expression
	: multiplicative_expression (('+' | '-')  multiplicative_expression)*
	;

multiplicative_expression
	: switch_expression (('*' | '/' | '%')  switch_expression)*
	;

switch_expression
    : range_expression ('switch' '{' (switch_expression_arms ','?)? '}')?
    ;

switch_expression_arms
    : switch_expression_arm (',' switch_expression_arm)*
    ;

switch_expression_arm
    : expression case_guard? right_arrow throwable_expression
    ;

range_expression
    : unary_expression
    | unary_expression? OP_RANGE unary_expression?
    ;

// https://msdn.microsoft.com/library/6a71f45d(v=vs.110).aspx
unary_expression
	: cast_expression
	| primary_expression
	| '+' unary_expression
	| '-' unary_expression
	| BANG unary_expression
	| '~' unary_expression
	| '++' unary_expression
	| '--' unary_expression
	| AWAIT unary_expression // C# 5
	| '&' unary_expression
	| '*' unary_expression
	| '^' unary_expression // C# 8 ranges
	;

cast_expression
    : OPEN_PARENS type_ CLOSE_PARENS unary_expression
    ;

primary_expression  // Null-conditional operators C# 6: https://msdn.microsoft.com/en-us/library/dn986595.aspx
	: pe=primary_expression_start '!'? bracket_expression* '!'?
	  ((member_access | method_invocation | '++' | '--' | '->' identifier) '!'? bracket_expression* '!'?)*
	;

primary_expression_start
	: literal                                   #literalExpression
	| identifier type_argument_list?            #simpleNameExpression
	| OPEN_PARENS expression CLOSE_PARENS       #parenthesisExpressions
	| predefined_type                           #memberAccessExpression
	| qualified_alias_member                    #memberAccessExpression
	| LITERAL_ACCESS                            #literalAccessExpression
	| THIS                                      #thisReferenceExpression
	| BASE ('.' identifier type_argument_list? | '[' expression_list ']') #baseAccessExpression
	| NEW (type_ (object_creation_expression
	             | object_or_collection_initializer
	             | '[' expression_list ']' rank_specifier* array_initializer?
	             | rank_specifier+ array_initializer)
	      | anonymous_object_initializer
	      | rank_specifier array_initializer)                       #objectCreationExpression
	| OPEN_PARENS argument ( ',' argument )+ CLOSE_PARENS           #tupleExpression
	| TYPEOF OPEN_PARENS (unbound_type_name | type_ | VOID) CLOSE_PARENS   #typeofExpression
	| CHECKED OPEN_PARENS expression CLOSE_PARENS                   #checkedExpression
	| UNCHECKED OPEN_PARENS expression CLOSE_PARENS                 #uncheckedExpression
	| DEFAULT (OPEN_PARENS type_ CLOSE_PARENS)?                     #defaultValueExpression
	| ASYNC? DELEGATE (OPEN_PARENS explicit_anonymous_function_parameter_list? CLOSE_PARENS)? block #anonymousMethodExpression
	| SIZEOF OPEN_PARENS type_ CLOSE_PARENS                          #sizeofExpression
	// C# 6: https://msdn.microsoft.com/en-us/library/dn986596.aspx
	| NAMEOF OPEN_PARENS (identifier '.')* identifier CLOSE_PARENS  #nameofExpression
	;

throwable_expression
	: expression
	| throw_expression
	;

throw_expression
	: THROW expression
	;

member_access
	: '?'? '.' identifier type_argument_list?
	;

bracket_expression
	: '?'? '[' indexer_argument ( ',' indexer_argument)* ']'
	;

indexer_argument
	: (identifier ':')? expression
	;

predefined_type
	: BOOL | BYTE | CHAR | DECIMAL | DOUBLE | FLOAT | INT | LONG
	| OBJECT | SBYTE | SHORT | STRING | UINT | ULONG | USHORT
	;

expression_list
	: expression (',' expression)*
	;

object_or_collection_initializer
	: object_initializer
	| collection_initializer
	;

object_initializer
	: OPEN_BRACE (member_initializer_list ','?)? CLOSE_BRACE
	;

member_initializer_list
	: member_initializer (',' member_initializer)*
	;

member_initializer
	: (identifier | '[' expression ']') '=' initializer_value // C# 6
	;

initializer_value
	: expression
	| object_or_collection_initializer
	;

collection_initializer
	: OPEN_BRACE element_initializer (',' element_initializer)* ','? CLOSE_BRACE
	;

element_initializer
	: non_assignment_expression
	| OPEN_BRACE expression_list CLOSE_BRACE
	;

anonymous_object_initializer
	: OPEN_BRACE (member_declarator_list ','?)? CLOSE_BRACE
	;

member_declarator_list
	: member_declarator ( ',' member_declarator)*
	;

member_declarator
	: primary_expression
	| identifier '=' expression
	;

unbound_type_name
	: identifier ( generic_dimension_specifier? | '::' identifier generic_dimension_specifier?)
	  ('.' identifier generic_dimension_specifier?)*
	;

generic_dimension_specifier
	: '<' ','* '>'
	;

isType
	: base_type (rank_specifier | '*')* '?'? isTypePatternArms? identifier?
	;

isTypePatternArms
	: '{' isTypePatternArm (',' isTypePatternArm)* '}'
	;

isTypePatternArm
	: identifier ':' expression
	;

lambda_expression
	: ASYNC? anonymous_function_signature right_arrow anonymous_function_body
	;

anonymous_function_signature
	: OPEN_PARENS CLOSE_PARENS
	| OPEN_PARENS explicit_anonymous_function_parameter_list CLOSE_PARENS
	| OPEN_PARENS implicit_anonymous_function_parameter_list CLOSE_PARENS
	| identifier
	;

explicit_anonymous_function_parameter_list
	: explicit_anonymous_function_parameter ( ',' explicit_anonymous_function_parameter)*
	;

explicit_anonymous_function_parameter
	: refout=(REF | OUT | IN)? type_ identifier
	;

implicit_anonymous_function_parameter_list
	: identifier (',' identifier)*
	;

anonymous_function_body
	: throwable_expression
	| block
	;

query_expression
	: from_clause query_body
	;

from_clause
	: FROM type_? identifier IN expression
	;

query_body
	: query_body_clause* select_or_group_clause query_continuation?
	;

query_body_clause
	: from_clause
	| let_clause
	| where_clause
	| combined_join_clause
	| orderby_clause
	;

let_clause
	: LET identifier '=' expression
	;

where_clause
	: WHERE expression
	;

combined_join_clause
	: JOIN type_? identifier IN expression ON expression EQUALS expression (INTO identifier)?
	;

orderby_clause
	: ORDERBY ordering (','  ordering)*
	;

ordering
	: expression dir=(ASCENDING | DESCENDING)?
	;

select_or_group_clause
	: SELECT expression
	| GROUP expression BY expression
	;

query_continuation
	: INTO identifier query_body
	;

//B.2.5 Statements
statement
	: labeled_Statement
	| declarationStatement
	| embedded_statement
	;

declarationStatement
	: local_variable_declaration ';'
	| local_constant_declaration ';'
	| local_function_declaration
	;

local_function_declaration
    : local_function_header local_function_body
    ;

local_function_header
    : local_function_modifiers? return_type identifier type_parameter_list?
        OPEN_PARENS formal_parameter_list? CLOSE_PARENS type_parameter_constraints_clauses?
    ;

local_function_modifiers
    : (ASYNC | UNSAFE) STATIC?
    | STATIC (ASYNC | UNSAFE)
    ;

local_function_body
    : block
    | right_arrow throwable_expression ';'
    ;

labeled_Statement
	: identifier ':' statement
	;

embedded_statement
	: block
	| simple_embedded_statement
	;

simple_embedded_statement
	: ';'                                                         #theEmptyStatement
	| expression ';'                                              #expressionStatement

	// selection statements
	| IF OPEN_PARENS expression CLOSE_PARENS if_body (ELSE if_body)?               #ifStatement
    | SWITCH OPEN_PARENS expression CLOSE_PARENS OPEN_BRACE switch_section* CLOSE_BRACE           #switchStatement

    // iteration statements
	| WHILE OPEN_PARENS expression CLOSE_PARENS embedded_statement                                        #whileStatement
	| DO embedded_statement WHILE OPEN_PARENS expression CLOSE_PARENS ';'                                 #doStatement
	| FOR OPEN_PARENS for_initializer? ';' expression? ';' for_iterator? CLOSE_PARENS embedded_statement  #forStatement
	| AWAIT? FOREACH OPEN_PARENS local_variable_type identifier IN expression CLOSE_PARENS embedded_statement    #foreachStatement

    // jump statements
	| BREAK ';'                                                   #breakStatement
	| CONTINUE ';'                                                #continueStatement
	| GOTO (identifier | CASE expression | DEFAULT) ';'           #gotoStatement
	| RETURN expression? ';'                                      #returnStatement
	| THROW expression? ';'                                       #throwStatement

	| TRY block (catch_clauses finally_clause? | finally_clause)  #tryStatement
	| CHECKED block                                               #checkedStatement
	| UNCHECKED block                                             #uncheckedStatement
	| LOCK OPEN_PARENS expression CLOSE_PARENS embedded_statement                  #lockStatement
	| USING OPEN_PARENS resource_acquisition CLOSE_PARENS embedded_statement       #usingStatement
	| YIELD (RETURN expression | BREAK) ';'                       #yieldStatement

	// unsafe statements
	| UNSAFE block                                                                       #unsafeStatement
	| FIXED OPEN_PARENS pointer_type fixed_pointer_declarators CLOSE_PARENS embedded_statement            #fixedStatement
	;

block
	: OPEN_BRACE statement_list? CLOSE_BRACE
	;

local_variable_declaration
	: (USING | REF | REF READONLY)? local_variable_type local_variable_declarator ( ','  local_variable_declarator { this.IsLocalVariableDeclaration() }? )*
	| FIXED pointer_type fixed_pointer_declarators
	;

local_variable_type
	: VAR
	| type_
	;

local_variable_declarator
	: identifier ('=' REF? local_variable_initializer )?
	;

local_variable_initializer
	: expression
	| array_initializer
	| stackalloc_initializer
	;

local_constant_declaration
	: CONST type_ constant_declarators
	;

if_body
	: block
	| simple_embedded_statement
	;

switch_section
	: switch_label+ statement_list
	;

switch_label
	: CASE expression case_guard? ':'
	| DEFAULT ':'
	;

case_guard
	: WHEN expression
	;

statement_list
	: statement+
	;

for_initializer
	: local_variable_declaration
	| expression (','  expression)*
	;

for_iterator
	: expression (','  expression)*
	;

catch_clauses
	: specific_catch_clause specific_catch_clause* general_catch_clause?
	| general_catch_clause
	;

specific_catch_clause
	: CATCH OPEN_PARENS class_type identifier? CLOSE_PARENS exception_filter? block
	;

general_catch_clause
	: CATCH exception_filter? block
	;

exception_filter // C# 6
	: WHEN OPEN_PARENS expression CLOSE_PARENS
	;

finally_clause
	: FINALLY block
	;

resource_acquisition
	: local_variable_declaration
	| expression
	;

//B.2.6 Namespaces;
namespace_declaration
	: NAMESPACE qi=qualified_identifier namespace_body ';'?
	;

qualified_identifier
	: identifier ( '.'  identifier )*
	;

namespace_body
	: OPEN_BRACE extern_alias_directives? using_directives? namespace_member_declarations? CLOSE_BRACE
	;

extern_alias_directives
	: extern_alias_directive+
	;

extern_alias_directive
	: EXTERN ALIAS identifier ';'
	;

using_directives
	: using_directive+
	;

using_directive
	: USING identifier '=' namespace_or_type_name ';'            #usingAliasDirective
	| USING namespace_or_type_name ';'                           #usingNamespaceDirective
	// C# 6: https://msdn.microsoft.com/en-us/library/ms228593.aspx
	| USING STATIC namespace_or_type_name ';'                    #usingStaticDirective
	;

namespace_member_declarations
	: namespace_member_declaration+
	;

namespace_member_declaration
	: namespace_declaration
	| type_declaration
	;

type_declaration
	: attributes? all_member_modifiers?
      (class_definition | struct_definition | interface_definition | enum_definition | delegate_definition)
  ;

qualified_alias_member
	: identifier '::' identifier type_argument_list?
	;

//B.2.7 Classes;
type_parameter_list
	: '<' type_parameter (','  type_parameter)* '>'
	;

type_parameter
	: attributes? identifier
	;

class_base
	: ':' class_type (','  namespace_or_type_name)*
	;

interface_type_list
	: namespace_or_type_name (','  namespace_or_type_name)*
	;

type_parameter_constraints_clauses
	: type_parameter_constraints_clause+
	;

type_parameter_constraints_clause
	: WHERE identifier ':' type_parameter_constraints
	;

type_parameter_constraints
	: constructor_constraint
	| primary_constraint (',' secondary_constraints)? (',' constructor_constraint)?
	;

primary_constraint
	: class_type
	| CLASS '?'?
	| STRUCT
	| UNMANAGED
	;

// namespace_or_type_name includes identifier
secondary_constraints
	: namespace_or_type_name (',' namespace_or_type_name)*
	;

constructor_constraint
	: NEW OPEN_PARENS CLOSE_PARENS
	;

class_body
	: OPEN_BRACE class_member_declarations? CLOSE_BRACE
	;

class_member_declarations
	: class_member_declaration+
	;

class_member_declaration
	: attributes? all_member_modifiers? (common_member_declaration | destructor_definition)
	;

all_member_modifiers
	: all_member_modifier+
	;

all_member_modifier
	: NEW
	| PUBLIC
	| PROTECTED
	| INTERNAL
	| PRIVATE
	| READONLY
	| VOLATILE
	| VIRTUAL
	| SEALED
	| OVERRIDE
	| ABSTRACT
	| STATIC
	| UNSAFE
	| EXTERN
	| PARTIAL
	| ASYNC  // C# 5
	;

// represents the intersection of struct_member_declaration and class_member_declaration
common_member_declaration
	: constant_declaration
	| typed_member_declaration
	| event_declaration
	| conversion_operator_declarator (body | right_arrow throwable_expression ';') // C# 6
	| constructor_declaration
	| VOID method_declaration
	| class_definition
	| struct_definition
	| interface_definition
	| enum_definition
	| delegate_definition
	;

typed_member_declaration
	: (REF | READONLY REF | REF READONLY)? type_
	  ( namespace_or_type_name '.' indexer_declaration
	  | method_declaration
	  | property_declaration
	  | indexer_declaration
	  | operator_declaration
	  | field_declaration
	  )
	;

constant_declarators
	: constant_declarator (','  constant_declarator)*
	;

constant_declarator
	: identifier '=' expression
	;

variable_declarators
	: variable_declarator (','  variable_declarator)*
	;

variable_declarator
	: identifier ('=' variable_initializer)?
	;

variable_initializer
	: expression
	| array_initializer
	;

return_type
	: type_
	| VOID
	;

member_name
	: namespace_or_type_name
	;

method_body
	: block
	| ';'
	;

formal_parameter_list
	: parameter_array
	| fixed_parameters (',' parameter_array)?
	;

fixed_parameters
	: fixed_parameter ( ',' fixed_parameter )*
	;

fixed_parameter
	: attributes? parameter_modifier? arg_declaration
	| ARGLIST
	;

parameter_modifier
	: REF
	| OUT
	| IN
	| REF THIS
	| IN THIS
	| THIS
	;

parameter_array
	: attributes? PARAMS array_type identifier
	;

accessor_declarations
	: attrs=attributes? mods=accessor_modifier?
	  (GET accessor_body set_accessor_declaration? | SET accessor_body get_accessor_declaration?)
	;

get_accessor_declaration
	: attributes? accessor_modifier? GET accessor_body
	;

set_accessor_declaration
	: attributes? accessor_modifier? SET accessor_body
	;

accessor_modifier
	: PROTECTED
	| INTERNAL
	| PRIVATE
	| PROTECTED INTERNAL
	| INTERNAL PROTECTED
	;

accessor_body
	: block
	| ';'
	;

event_accessor_declarations
	: attributes? (ADD block remove_accessor_declaration | REMOVE block add_accessor_declaration)
	;

add_accessor_declaration
	: attributes? ADD block
	;

remove_accessor_declaration
	: attributes? REMOVE block
	;

overloadable_operator
	: '+'
	| '-'
	| BANG
	| '~'
	| '++'
	| '--'
	| TRUE
	| FALSE
	| '*'
	| '/'
	| '%'
	| '&'
	| '|'
	| '^'
	| '<<'
	| right_shift
	| OP_EQ
	| OP_NE
	| '>'
	| '<'
	| '>='
	| '<='
	;

conversion_operator_declarator
	: (IMPLICIT | EXPLICIT) OPERATOR type_ OPEN_PARENS arg_declaration CLOSE_PARENS
	;

constructor_initializer
	: ':' (BASE | THIS) OPEN_PARENS argument_list? CLOSE_PARENS
	;

body
	: block
	| ';'
	;

//B.2.8 Structs
struct_interfaces
	: ':' interface_type_list
	;

struct_body
	: OPEN_BRACE struct_member_declaration* CLOSE_BRACE
	;

struct_member_declaration
	: attributes? all_member_modifiers?
	  (common_member_declaration | FIXED type_ fixed_size_buffer_declarator+ ';')
	;

//B.2.9 Arrays
array_type
	: base_type (('*' | '?')* rank_specifier)+
	;

rank_specifier
	: '[' ','* ']'
	;

array_initializer
	: OPEN_BRACE (variable_initializer (','  variable_initializer)* ','?)? CLOSE_BRACE
	;

//B.2.10 Interfaces
variant_type_parameter_list
	: '<' variant_type_parameter (',' variant_type_parameter)* '>'
	;

variant_type_parameter
	: attributes? variance_annotation? identifier
	;

variance_annotation
	: IN | OUT
	;

interface_base
	: ':' interface_type_list
	;

interface_body // ignored in csharp 8
	: OPEN_BRACE interface_member_declaration* CLOSE_BRACE
	;

interface_member_declaration
	: attributes? NEW?
	  (UNSAFE? (REF | REF READONLY | READONLY REF)? type_
	    ( identifier type_parameter_list? OPEN_PARENS formal_parameter_list? CLOSE_PARENS type_parameter_constraints_clauses? ';'
	    | identifier OPEN_BRACE interface_accessors CLOSE_BRACE
	    | THIS '[' formal_parameter_list ']' OPEN_BRACE interface_accessors CLOSE_BRACE)
	  | UNSAFE? VOID identifier type_parameter_list? OPEN_PARENS formal_parameter_list? CLOSE_PARENS type_parameter_constraints_clauses? ';'
	  | EVENT type_ identifier ';')
	;

interface_accessors
	: attributes? (GET ';' (attributes? SET ';')? | SET ';' (attributes? GET ';')?)
	;

//B.2.11 Enums
enum_base
	: ':' type_
	;

enum_body
	: OPEN_BRACE (enum_member_declaration (','  enum_member_declaration)* ','?)? CLOSE_BRACE
	;

enum_member_declaration
	: attributes? identifier ('=' expression)?
	;

//B.2.12 Delegates

//B.2.13 Attributes
global_attribute_section
	: '[' global_attribute_target ':' attribute_list ','? ']'
	;

global_attribute_target
	: keyword
	| identifier
	;

attributes
	: attribute_section+
	;

attribute_section
	: '[' (attribute_target ':')? attribute_list ','? ']'
	;

attribute_target
	: keyword
	| identifier
	;

attribute_list
	: attribute (','  attribute)*
	;

attribute
	: namespace_or_type_name (OPEN_PARENS (attribute_argument (','  attribute_argument)*)? CLOSE_PARENS)?
	;

attribute_argument
	: (identifier ':')? expression
	;

//B.3 Grammar extensions for unsafe code
pointer_type
	: (simple_type | class_type) (rank_specifier | '?')* '*'
	| VOID '*'
	;

fixed_pointer_declarators
	: fixed_pointer_declarator (','  fixed_pointer_declarator)*
	;

fixed_pointer_declarator
	: identifier '=' fixed_pointer_initializer
	;

fixed_pointer_initializer
	: '&'? expression
	| stackalloc_initializer
	;

fixed_size_buffer_declarator
	: identifier '[' expression ']'
	;

stackalloc_initializer
	: STACKALLOC type_ '[' expression ']'
	| STACKALLOC type_? '[' expression? ']' OPEN_BRACE expression (',' expression)* ','? CLOSE_BRACE
	;

right_arrow
	: first='=' second='>' {$first.index + 1 == $second.index}? // Nothing between the tokens?
	;

right_shift
	: first='>' second='>' {$first.index + 1 == $second.index}? // Nothing between the tokens?
	;

right_shift_assignment
	: first='>' second='>=' {$first.index + 1 == $second.index}? // Nothing between the tokens?
	;

literal
	: boolean_literal
	| string_literal
	| INTEGER_LITERAL
	| HEX_INTEGER_LITERAL
	| BIN_INTEGER_LITERAL
	| REAL_LITERAL
	| CHARACTER_LITERAL
	| NULL_
	;

boolean_literal
	: TRUE
	| FALSE
	;

string_literal
	: interpolated_regular_string
	| interpolated_verbatium_string
	| REGULAR_STRING
	| VERBATIUM_STRING
	;

interpolated_regular_string
	: INTERPOLATED_REGULAR_STRING_START interpolated_regular_string_part* DOUBLE_QUOTE_INSIDE
	;


interpolated_verbatium_string
	: INTERPOLATED_VERBATIUM_STRING_START interpolated_verbatium_string_part* DOUBLE_QUOTE_INSIDE
	;

interpolated_regular_string_part
	: interpolated_string_expression
	| DOUBLE_CURLY_INSIDE
	| REGULAR_CHAR_INSIDE
	| REGULAR_STRING_INSIDE
	;

interpolated_verbatium_string_part
	: interpolated_string_expression
	| DOUBLE_CURLY_INSIDE
	| VERBATIUM_DOUBLE_QUOTE_INSIDE
	| VERBATIUM_INSIDE_STRING
	;

interpolated_string_expression
	: expression (',' expression)* (':' FORMAT_STRING+)?
	;

//B.1.7 Keywords
keyword
	: ABSTRACT
	| AS
	| BASE
	| BOOL
	| BREAK
	| BYTE
	| CASE
	| CATCH
	| CHAR
	| CHECKED
	| CLASS
	| CONST
	| CONTINUE
	| DECIMAL
	| DEFAULT
	| DELEGATE
	| DO
	| DOUBLE
	| ELSE
	| ENUM
	| EVENT
	| EXPLICIT
	| EXTERN
	| FALSE
	| FINALLY
	| FIXED
	| FLOAT
	| FOR
	| FOREACH
	| GOTO
	| IF
	| IMPLICIT
	| IN
	| INT
	| INTERFACE
	| INTERNAL
	| IS
	| LOCK
	| LONG
	| NAMESPACE
	| NEW
	| NULL_
	| OBJECT
	| OPERATOR
	| OUT
	| OVERRIDE
	| PARAMS
	| PRIVATE
	| PROTECTED
	| PUBLIC
	| READONLY
	| REF
	| RETURN
	| SBYTE
	| SEALED
	| SHORT
	| SIZEOF
	| STACKALLOC
	| STATIC
	| STRING
	| STRUCT
	| SWITCH
	| THIS
	| THROW
	| TRUE
	| TRY
	| TYPEOF
	| UINT
	| ULONG
	| UNCHECKED
	| UNMANAGED
	| UNSAFE
	| USHORT
	| USING
	| VIRTUAL
	| VOID
	| VOLATILE
	| WHILE
	;

// -------------------- extra rules for modularization --------------------------------

class_definition
	: CLASS identifier type_parameter_list? class_base? type_parameter_constraints_clauses?
	    class_body ';'?
	;

struct_definition
	: (READONLY | REF)? STRUCT identifier type_parameter_list? struct_interfaces? type_parameter_constraints_clauses?
	    struct_body ';'?
	;

interface_definition
	: INTERFACE identifier variant_type_parameter_list? interface_base?
	    type_parameter_constraints_clauses? class_body ';'?
	;

enum_definition
	: ENUM identifier enum_base? enum_body ';'?
	;

delegate_definition
	: DELEGATE return_type identifier variant_type_parameter_list?
	  OPEN_PARENS formal_parameter_list? CLOSE_PARENS type_parameter_constraints_clauses? ';'
	;

event_declaration
	: EVENT type_ (variable_declarators ';' | member_name OPEN_BRACE event_accessor_declarations CLOSE_BRACE)
	;

field_declaration
	: variable_declarators ';'
	;

property_declaration // Property initializer & lambda in properties C# 6
	: member_name (OPEN_BRACE accessor_declarations CLOSE_BRACE ('=' variable_initializer ';')? | right_arrow throwable_expression ';')
	;

constant_declaration
	: CONST type_ constant_declarators ';'
	;

indexer_declaration // lamdas from C# 6
	: THIS '[' formal_parameter_list ']' (OPEN_BRACE accessor_declarations CLOSE_BRACE | right_arrow throwable_expression ';')
	;

destructor_definition
	: '~' identifier OPEN_PARENS CLOSE_PARENS body
	;

constructor_declaration
	: identifier OPEN_PARENS formal_parameter_list? CLOSE_PARENS constructor_initializer? body
	;

method_declaration // lamdas from C# 6
	: method_member_name type_parameter_list? OPEN_PARENS formal_parameter_list? CLOSE_PARENS
	    type_parameter_constraints_clauses? (method_body | right_arrow throwable_expression ';')
	;

method_member_name
	: (identifier | identifier '::' identifier) (type_argument_list? '.' identifier)*
	;

operator_declaration // lamdas form C# 6
	: OPERATOR overloadable_operator OPEN_PARENS IN? arg_declaration
	       (',' IN? arg_declaration)? CLOSE_PARENS (body | right_arrow throwable_expression ';')
	;

arg_declaration
	: type_ identifier ('=' expression)?
	;

method_invocation
	: OPEN_PARENS argument_list? CLOSE_PARENS
	;

object_creation_expression
	: OPEN_PARENS argument_list? CLOSE_PARENS object_or_collection_initializer?
	;

identifier
	: IDENTIFIER
	| ADD
	| ALIAS
	| ARGLIST
	| ASCENDING
	| ASYNC
	| AWAIT
	| BY
	| DESCENDING
	| DYNAMIC
	| EQUALS
	| FROM
	| GET
	| GROUP
	| INTO
	| JOIN
	| LET
	| NAMEOF
	| ON
	| ORDERBY
	| PARTIAL
	| REMOVE
	| SELECT
	| SET
	| UNMANAGED
	| VAR
	| WHEN
	| WHERE
	| YIELD
	;