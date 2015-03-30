grammar CSharp4;

import CSharp4PreProcessor;

//B.2 Syntactic grammar

//B.2.1 Basic concepts
namespace_name 
	: namespace_or_type_name
	;
type_name 
	: namespace_or_type_name
	;

identifier:
     IDENTIFIER 
    | FROM
    | LET 
    | WHERE 
    | JOIN 
    | ON 
    | EQUALS 
    | INTO
    | ORDERBY
    | ASCENDING
    | DESCENDING
    | SELECT
    | GROUP
    | BY
    | PARTIAL
    | ALIAS
    | YIELD
    | GET
    | SET
    | ADD
    | REMOVE
    | DYNAMIC
    | ARGLIST
    ;

namespace_or_type_name 
  : ( identifier type_argument_list_opt
    | qualified_alias_member
    ) (DOT identifier type_argument_list_opt )*
  ;

/** represents type_argument_list? */
type_argument_list_opt
  : type_argument_list
  | /* empty */
  ;

//B.2.2 Types
type 
  : base_type (INTERR | rank_specifier | STAR)*
  ;
// added by chw
base_type
  : simple_type
  | class_type  // represents types: enum, class, interface, delegate, type_parameter
  | VOID STAR
  ;
/*
value_type 
	: struct_type
	| enum_type
	;
struct_type 
	: type_name
	| simple_type
	| nullable_type
	;
*/
/** primitive types */
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
nullable_type 
	: non_nullable_value_type INTERR
	;
/*
non_nullable_value_type 
  : type
  ;
*/
// type without INTERR; undocumented but VS checks for this constraint
non_nullable_value_type 
	: base_type
    ( rank_specifier
    | STAR
    )*
	;
/* not used anymore
enum_type 
	: type_name
	;
*/
/*
reference_type 
	: class_type
	| interface_type
	| array_type
	| delegate_type
	;
*/
reference_type 
  : simple_type ((STAR | INTERR)* rank_specifier)* (STAR | INTERR)* rank_specifier 
  | class_type ((STAR | INTERR)* rank_specifier)*
  | VOID STAR ((STAR | INTERR)* rank_specifier)* (STAR | INTERR)* rank_specifier
  ;
/** type_name, OBJECT, STRING */
class_type 
	: type_name
	| OBJECT
	| dynamic_contextual_keyword
	| STRING
	;
/** type_name */
interface_type 
	: type_name
	;
/** type_name */
delegate_type 
	: type_name
	;
type_argument_list 
	: LT type_arguments GT
	;
type_arguments 
	: type_argument ( COMMA type_argument)*
	;
type_argument 
	: type
	;
// added by chw
type_void
  : VOID
  ;

//B.2.3 Variables
/** expression */
variable_reference 
	: expression
	;

//B.2.4 Expressions
argument_list 
	: argument ( COMMA argument)*
	;
argument
	: argument_name? argument_value
	;
argument_name 
	: identifier COLON
	;
argument_value 
	: expression
	| REF variable_reference
	| OUT variable_reference
	;
/*
primary_expression 
	: primary_no_array_creation_expression
	| array_creation_expression
	;
*/
primary_expression 
  : pe=primary_expression_start  bracket_expression* (
        ( member_access2
		    | method_invocation2
		    | OP_INC
		    | OP_DEC
		    | OP_PTR identifier 
		    )
		    bracket_expression*
		)*
  ;
primary_expression_start
  : literal
  | simple_name
  | parenthesized_expression
  | predefined_type // member_access
  | qualified_alias_member  // member_access
  | this_access
  | base_access
  | NEW ( type ( object_creation_expression2
               | object_or_collection_initializer
               | OPEN_BRACKET expression_list CLOSE_BRACKET rank_specifiers? array_initializer?
               | rank_specifiers array_initializer
               )
        | anonymous_object_initializer
        | rank_specifier array_initializer
        )
  | typeof_expression
  | checked_expression
  | unchecked_expression
  | default_value_expression
  | anonymous_method_expression
  | sizeof_expression
  ;
/*
bracket_expression
  : OPEN_BRACKET expression_list CLOSE_BRACKET
  | OPEN_BRACKET expression CLOSE_BRACKET
  ;
*/
bracket_expression
  : OPEN_BRACKET expression_list CLOSE_BRACKET
  ;
/*
primary_no_array_creation_expression 
	: literal
	| simple_name
	| parenthesized_expression
	| member_access
	| invocation_expression
	| element_access
	| this_access
	| base_access
	| post_increment_expression
	| post_decrement_expression
	| object_creation_expression
	| delegate_creation_expression
	| anonymous_object_creation_expression
	| typeof_expression
	| checked_expression
	| unchecked_expression
	| default_value_expression
	| anonymous_method_expression
	| primary_no_array_creation_expression_unsafe
	;
*/
/*
primary_no_array_creation_expression 
	: ( literal
		| simple_name
		| parenthesized_expression
		| new_expression
		// member_access
    | predefined_type DOT identifier type_argument_list?
    | qualified_alias_member DOT identifier
    | this_access
    | base_access
		| typeof_expression
		| checked_expression
		| unchecked_expression
		| default_value_expression
		| anonymous_method_expression
		// pointer_element_access
	  | sizeof_expression
	  ) ( DOT identifier type_argument_list?
      | OPEN_PARENS argument_list? CLOSE_PARENS
      | OPEN_BRACKET expression_list CLOSE_BRACKET
      | OP_INC
      | OP_DEC
      | OP_PTR identifier
      )*
	;
new_expression
  : NEW ( type ( OPEN_BRACKET expression_list CLOSE_BRACKET rank_specifiers? array_initializer? array_creation_tail
				       | rank_specifiers array_initializer array_creation_tail
				       | OPEN_PARENS argument_list? CLOSE_PARENS object_or_collection_initializer? // inludes delegate
				       | object_or_collection_initializer
				       )
	      | rank_specifier array_initializer array_creation_tail
	      | anonymous_object_initializer
	      )
  ;
array_creation_tail
    // member_access
  : DOT identifier type_argument_list?
    // invocation_expression (but only one possibility)
  | OPEN_PARENS argument_list? CLOSE_PARENS
    // post inc
  | OP_INC
    // post dec
  | OP_DEC
    // pointer_member_access
  | OP_PTR identifier
  ;
*/
/** identifier type_argument_list? */
simple_name 
	: identifier type_argument_list_opt
	;
/** OPEN_PARENS expression CLOSE_PARENS */
parenthesized_expression 
	: OPEN_PARENS expression CLOSE_PARENS
	;
/*
member_access 
	: primary_expression DOT identifier type_argument_list?
	| predefined_type DOT identifier type_argument_list?
	| qualified_alias_member DOT identifier type_argument_list?
	;
*/
/** primary_expression */
member_access 
  : primary_expression
  ;
predefined_type 
	: BOOL
	| BYTE
	| CHAR
	| DECIMAL
	| DOUBLE
	| FLOAT
	| INT
	| LONG
	| OBJECT
	| SBYTE
	| SHORT
	| STRING
	| UINT
	| ULONG
	| USHORT
	;
/** primary_expression OPEN_PARENS argument_list? CLOSE_PARENS */
/* not used anymore; included in primary_expression
invocation_expression 
	: primary_expression OPEN_PARENS argument_list? CLOSE_PARENS
	;
*/
/*
element_access 
	: primary_no_array_creation_expression OPEN_BRACKET expression_list CLOSE_BRACKET
	;
*/
expression_list 
	: expression ( COMMA expression)*
	;
this_access 
	: THIS
	;
/** BASE and more */
base_access
	: BASE DOT identifier type_argument_list_opt
	| BASE OPEN_BRACKET expression_list CLOSE_BRACKET
	;
/* not used anymore; included in primary_expression
post_increment_expression 
	: primary_expression OP_INC
	;
post_decrement_expression 
	: primary_expression OP_DEC
	;
*/
/*
object_creation_expression 
	: NEW type OPEN_PARENS argument_list? CLOSE_PARENS object_or_collection_initializer?
	| NEW type object_or_collection_initializer
	;
*/
/** NEW type (OPEN_PARENS ... | OPEN_BRACE ...) */
object_creation_expression 
  : NEW type ( OPEN_PARENS argument_list? CLOSE_PARENS object_or_collection_initializer?
             | object_or_collection_initializer
             )
  ;
/** starts with OPEN_BRACE */
object_or_collection_initializer 
	: object_initializer
	| collection_initializer
	;
/*
object_initializer 
	: OPEN_BRACE member_initializer_list? CLOSE_BRACE
	| OPEN_BRACE member_initializer_list COMMA CLOSE_BRACE
	;
*/
/** starts with OPEN_BRACE */
object_initializer 
  : OPEN_BRACE CLOSE_BRACE
  | OPEN_BRACE member_initializer_list COMMA? CLOSE_BRACE
  ;
member_initializer_list 
	: member_initializer ( COMMA member_initializer)*
	;
member_initializer 
	: identifier ASSIGNMENT initializer_value
	;
initializer_value 
	: expression
	| object_or_collection_initializer
	;
/*
collection_initializer 
	: OPEN_BRACE element_initializer_list CLOSE_BRACE
	| OPEN_BRACE element_initializer_list COMMA CLOSE_BRACE
	;
*/
/** starts with OPEN_BRACE */
collection_initializer 
  : OPEN_BRACE element_initializer_list COMMA? CLOSE_BRACE
  ;
element_initializer_list 
	: element_initializer ( COMMA element_initializer)*
	;
element_initializer 
	: non_assignment_expression
	| OPEN_BRACE expression_list CLOSE_BRACE
	;
/*
array_creation_expression 
	: NEW non_array_type OPEN_BRACKET expression_list CLOSE_BRACKET rank_specifiers? array_initializer?
	| NEW array_type array_initializer
	| NEW rank_specifier array_initializer
	;
*/
array_creation_expression 
  : NEW ( array_type array_initializer
        | non_array_type OPEN_BRACKET expression_list CLOSE_BRACKET rank_specifiers? array_initializer?
        | rank_specifier array_initializer
        )
  ;
/** NEW delegate_type OPEN_PARENS expression CLOSE_PARENS */
delegate_creation_expression 
	: NEW delegate_type OPEN_PARENS expression CLOSE_PARENS
	;
/** starts with NEW OPEN_BRACE */
anonymous_object_creation_expression 
	: NEW anonymous_object_initializer
	;
/*
anonymous_object_initializer 
	: OPEN_BRACE member_declarator_list? CLOSE_BRACE
	| OPEN_BRACE member_declarator_list COMMA CLOSE_BRACE
	;
*/
/** starts with OPEN_BRACE */
anonymous_object_initializer 
  : OPEN_BRACE CLOSE_BRACE
  | OPEN_BRACE member_declarator_list COMMA? CLOSE_BRACE
  ;
member_declarator_list 
	: member_declarator ( COMMA member_declarator)*
	;
/*
member_declarator 
	: simple_name
	| member_access
	| base_access
	| identifier ASSIGNMENT expression
	;
*/
member_declarator 
  : primary_expression
  | identifier ASSIGNMENT expression
  ;
typeof_expression 
	: TYPEOF OPEN_PARENS
	  ( unbound_type_name CLOSE_PARENS
	  | type CLOSE_PARENS
	  | VOID CLOSE_PARENS
	  )
	;
/*
unbound_type_name 
	: identifier generic_dimension_specifier?
	| identifier DOUBLE_COLON identifier generic_dimension_specifier?
	| unbound_type_name DOT identifier generic_dimension_specifier?
	;
*/
unbound_type_name 
  : identifier ( generic_dimension_specifier?
               | DOUBLE_COLON identifier generic_dimension_specifier?
               )
    (DOT identifier generic_dimension_specifier?)*
  ;
generic_dimension_specifier 
	: LT commas? GT
	;
commas 
	: COMMA ( COMMA )*
	;
checked_expression 
	: CHECKED OPEN_PARENS expression CLOSE_PARENS
	;
unchecked_expression 
	: UNCHECKED OPEN_PARENS expression CLOSE_PARENS
	;
default_value_expression 
	: DEFAULT OPEN_PARENS type CLOSE_PARENS
	;
/*
unary_expression 
	: primary_expression
	| PLUS unary_expression
	| MINUS unary_expression
	| BANG unary_expression
	| TILDE unary_expression
	| pre_increment_expression
	| pre_decrement_expression
	| cast_expression
	| unary_expression_unsafe
	;
*/
unary_expression 
	: cast_expression
	| primary_expression
	| PLUS unary_expression
	| MINUS unary_expression
	| BANG unary_expression
	| TILDE unary_expression
	| pre_increment_expression
	| pre_decrement_expression
	| unary_expression_unsafe
	;
// The sequence of tokens is correct grammar for a type, and the token immediately
// following the closing parentheses is the token TILDE, the token BANG, the token OPEN_PARENS,
// an identifier, a literal, or any keyword except AS and IS.
scan_for_cast_generic_precedence
  : OPEN_PARENS type CLOSE_PARENS cast_disambiguation_token
  ;

// One of these tokens must follow a valid cast in an expression, in order to
// eliminate a grammar ambiguity.
cast_disambiguation_token
  : (TILDE | BANG | OPEN_PARENS | identifier | literal | ABSTRACT | BASE | BOOL | BREAK | BYTE | CASE | CATCH
    | CHAR | CHECKED | CLASS | CONST | CONTINUE | DECIMAL | DEFAULT | DELEGATE | DO | DOUBLE | ELSE | ENUM
    | EVENT | EXPLICIT | EXTERN | FINALLY | FIXED | FLOAT | FOR | FOREACH | GOTO | IF | IMPLICIT | IN | INT
    | INTERFACE | INTERNAL | LOCK | LONG | NAMESPACE | NEW | OBJECT | OPERATOR | OUT | OVERRIDE | PARAMS
    | PRIVATE | PROTECTED | PUBLIC | READONLY | REF | RETURN | SBYTE | SEALED | SHORT | SIZEOF | STACKALLOC
    | STATIC | STRING | STRUCT | SWITCH | THIS | THROW | TRY | TYPEOF | UINT | ULONG | UNCHECKED | UNSAFE
    | USHORT | USING | VIRTUAL | VOID | VOLATILE | WHILE
    )
  ;
pre_increment_expression 
	: OP_INC unary_expression
	;
pre_decrement_expression 
	: OP_DEC unary_expression
	;
cast_expression 
	: OPEN_PARENS type CLOSE_PARENS unary_expression
	;
multiplicative_expression 
	: unary_expression ( ( STAR  unary_expression ) | ( DIV  unary_expression ) | ( PERCENT  unary_expression ) )*
	;
additive_expression 
	: multiplicative_expression ( ( PLUS  multiplicative_expression ) | ( MINUS  multiplicative_expression ) )*
	;
shift_expression 
	: additive_expression ( ( OP_LEFT_SHIFT  additive_expression ) | ( right_shift  additive_expression ) )*
	;
relational_expression
  : /* shift_expression
	|*/ shift_expression ( LT shift_expression
                     | GT shift_expression
                     | OP_LE shift_expression
                     | OP_GE shift_expression
                     | IS isType
                     | AS type
	                   )*
	;
// Syntactic predicate rule to disambiguate  "a<b,c>d" and a<b,c>(0)"
// added by chw
scan_for_shift_generic_precedence
  : identifier LT type (COMMA type)* GT shift_disambiguation_token
  ;
// One of these tokens must follow a valid generic in an expression, in order to
// eliminate a grammar ambiguity.
// added by chw
shift_disambiguation_token
  : OPEN_PARENS | CLOSE_PARENS | CLOSE_BRACKET | COLON | SEMICOLON | COMMA | DOT | INTERR | OP_EQ | OP_NE | GT
  ;
// added by chw
isType
  : non_nullable_value_type ( INTERR)?
  ;
is_disambiguation_token
  : CLOSE_PARENS | OP_AND | OP_OR| INTERR
  ;
equality_expression 
	: relational_expression ( ( OP_EQ  relational_expression ) | ( OP_NE  relational_expression ) )*
	;
and_expression 
	: equality_expression ( AMP equality_expression)*
	;
exclusive_or_expression 
	: and_expression ( CARET and_expression)*
	;
inclusive_or_expression 
	: exclusive_or_expression ( BITWISE_OR exclusive_or_expression)*
	;
conditional_and_expression 
	: inclusive_or_expression ( OP_AND inclusive_or_expression)*
	;
conditional_or_expression 
	: conditional_and_expression ( OP_OR conditional_and_expression)*
	;
/*
null_coalescing_expression 
	: conditional_or_expression
	| conditional_or_expression OP_COALESCING null_coalescing_expression
	;
*/
null_coalescing_expression 
  : conditional_or_expression (OP_COALESCING null_coalescing_expression)?
  ;
/*
conditional_expression 
	: null_coalescing_expression
	| null_coalescing_expression INTERR expression COLON expression
	;
*/
/** starts with unary_expression */
conditional_expression 
  : null_coalescing_expression (INTERR expression COLON expression)?
  ;
/** starts with OPEN_PARENS or identifier */
lambda_expression 
	: anonymous_function_signature right_arrow anonymous_function_body
	;
/** starts with DELEGATE */
anonymous_method_expression 
	: DELEGATE explicit_anonymous_function_signature? block
	;
/*
anonymous_function_signature 
	: explicit_anonymous_function_signature
	| implicit_anonymous_function_signature
	;
*/
/** starts with OPEN_PARENS or identifier */
anonymous_function_signature 
  : OPEN_PARENS CLOSE_PARENS
  | OPEN_PARENS explicit_anonymous_function_parameter_list CLOSE_PARENS
  | OPEN_PARENS implicit_anonymous_function_parameter_list CLOSE_PARENS
  | implicit_anonymous_function_parameter
  ;
explicit_anonymous_function_signature 
	: OPEN_PARENS explicit_anonymous_function_parameter_list? CLOSE_PARENS
	;
explicit_anonymous_function_parameter_list 
	: explicit_anonymous_function_parameter ( COMMA explicit_anonymous_function_parameter)*
	;
explicit_anonymous_function_parameter 
	: anonymous_function_parameter_modifier? type identifier
	;
anonymous_function_parameter_modifier 
	: REF
	| OUT
	;
implicit_anonymous_function_signature 
	: OPEN_PARENS implicit_anonymous_function_parameter_list? CLOSE_PARENS
	| implicit_anonymous_function_parameter
	;
implicit_anonymous_function_parameter_list 
	: implicit_anonymous_function_parameter ( COMMA implicit_anonymous_function_parameter)*
	;
/** identifier */
implicit_anonymous_function_parameter 
	: identifier
	;
anonymous_function_body 
	: expression
	| block
	;
/** starts with from_contextual_keyword */
query_expression 
	: from_clause query_body
	;
from_clause 
	: from_contextual_keyword type? identifier IN expression
	;
/*
query_body 
	: query_body_clauses? select_or_group_clause query_continuation?
	;
*/
query_body 
  : query_body_clauses? select_or_group_clause query_continuation?
  ;
query_body_clauses 
	: query_body_clause ( query_body_clause )*
	;
/*
query_body_clause 
	: from_clause
	| let_clause
	| where_clause
	| join_clause
	| join_into_clause
	| orderby_clause
	;
*/
query_body_clause 
  : from_clause
  | let_clause
  | where_clause
  | combined_join_clause
  | orderby_clause
  ;
let_clause 
	: let_contextual_keyword identifier ASSIGNMENT expression
	;
where_clause 
	: where_contextual_keyword boolean_expression
	;
join_clause 
	: join_contextual_keyword type? identifier IN expression on_contextual_keyword expression equals_contextual_keyword expression
	;
join_into_clause 
	: join_contextual_keyword type? identifier IN expression on_contextual_keyword expression equals_contextual_keyword expression into_contextual_keyword identifier
	;
// added by chw
combined_join_clause
  : join_contextual_keyword type? identifier IN expression on_contextual_keyword expression equals_contextual_keyword expression (into_contextual_keyword identifier)?
  ;
orderby_clause 
	: orderby_contextual_keyword orderings
	;
orderings 
	: ordering ( COMMA  ordering )*
	;
ordering 
	: expression ordering_direction?
	;
ordering_direction 
	: ascending_contextual_keyword
	| descending_contextual_keyword
	;
select_or_group_clause 
	: select_clause
	| group_clause
	;
select_clause 
	: select_contextual_keyword expression
	;
group_clause 
	: group_contextual_keyword expression by_contextual_keyword expression
	;
/** starts with into_contextual_keyword */
query_continuation 
	: into_contextual_keyword identifier query_body
	;
/** starts with unary_expression */
assignment 
	: unary_expression assignment_operator expression
	;
assignment_operator 
	: ASSIGNMENT
	| OP_ADD_ASSIGNMENT
	| OP_SUB_ASSIGNMENT
	| OP_MULT_ASSIGNMENT
	| OP_DIV_ASSIGNMENT
	| OP_MOD_ASSIGNMENT
	| OP_AND_ASSIGNMENT
	| OP_OR_ASSIGNMENT
	| OP_XOR_ASSIGNMENT
	| OP_LEFT_SHIFT_ASSIGNMENT
	| right_shift_assignment
	;
expression 
	: assignment
	| non_assignment_expression
	;
non_assignment_expression
	: lambda_expression
	| query_expression
	| conditional_expression
	;
constant_expression 
	: expression
	;
boolean_expression 
	: expression
	;

//B.2.5 Statements
statement 
	: labeled_statement
	| declaration_statement
	| embedded_statement
	;
embedded_statement 
	: block
	| simple_embedded_statement
	;
simple_embedded_statement 
	: empty_statement
	| expression_statement
	| selection_statement
	| iteration_statement
	| jump_statement
	| try_statement
	| checked_statement
	| unchecked_statement
	| lock_statement
	| using_statement
	| yield_statement
	| embedded_statement_unsafe
	;
/** starts with OPEN_BRACE */
block 
	: OPEN_BRACE statement_list? CLOSE_BRACE
	;
statement_list 
	: statement+
	;
empty_statement 
	: SEMICOLON
	;
/** starts with identifier COLON */
labeled_statement 
	: identifier COLON statement
	;
/** starts with type, VAR, or CONST */
declaration_statement 
	: local_variable_declaration SEMICOLON
	| local_constant_declaration SEMICOLON
	;
local_variable_declaration 
	: t=local_variable_type  local_variable_declarators
	;
local_variable_type 
	: type // includes 'var'
	;
/** starts with identifier */
local_variable_declarators 
	: local_variable_declarator ( COMMA  local_variable_declarator )*
	;
local_variable_declarator 
  : identifier (ASSIGNMENT local_variable_initializer)?
  ;
local_variable_initializer
	: expression
	| array_initializer
	| local_variable_initializer_unsafe
	;
local_constant_declaration 
	: CONST type constant_declarators
	;
expression_statement
	: statement_expression SEMICOLON
	;
statement_expression 
	: expression;
selection_statement 
	: if_statement
	| switch_statement
	;

if_body
        : block                     # ifBodyBlock
        | simple_embedded_statement # ifBodySingle
        ;

if_statement 
        : IF OPEN_PARENS boolean_expression CLOSE_PARENS if_body (ELSE if_body)?
        ;
switch_statement 
	: SWITCH OPEN_PARENS expression CLOSE_PARENS switch_block
	;
switch_block 
	: OPEN_BRACE switch_sections? CLOSE_BRACE
	;
switch_sections 
	: switch_section ( switch_section )*
	;
switch_section 
	: switch_labels statement_list
	;
switch_labels 
	: switch_label ( switch_label )*
	;
switch_label 
	: CASE constant_expression COLON
	| DEFAULT COLON
	;
iteration_statement 
	: while_statement
	| do_statement
	| for_statement
	| foreach_statement
	;
while_statement 
	: WHILE OPEN_PARENS boolean_expression CLOSE_PARENS embedded_statement
	;
do_statement 
	: DO embedded_statement WHILE OPEN_PARENS boolean_expression CLOSE_PARENS SEMICOLON
	;
for_statement 
	: FOR OPEN_PARENS for_initializer? SEMICOLON for_condition? SEMICOLON for_iterator? CLOSE_PARENS embedded_statement
	;
for_initializer 
	: local_variable_declaration
	| statement_expression_list
	;
for_condition 
	: boolean_expression
	;
for_iterator 
	: statement_expression_list
	;
statement_expression_list 
	: statement_expression ( COMMA  statement_expression )*
	;
foreach_statement 
	: FOREACH OPEN_PARENS local_variable_type identifier IN expression CLOSE_PARENS embedded_statement
	;
jump_statement 
	: break_statement
	| continue_statement
	| goto_statement
	| return_statement
	| throw_statement
	;
break_statement 
	: BREAK SEMICOLON
	;
continue_statement 
	: CONTINUE SEMICOLON
	;
goto_statement 
	: GOTO identifier SEMICOLON
	| GOTO CASE constant_expression SEMICOLON
	| GOTO DEFAULT SEMICOLON
	;
return_statement 
	: RETURN expression? SEMICOLON
	;
throw_statement 
	: THROW expression? SEMICOLON
	;
/*
try_statement 
	: TRY block catch_clauses
	| TRY block finally_clause
	| TRY block catch_clauses finally_clause
	;
*/
try_statement 
  : TRY block catch_clauses? finally_clause?
  ;
/*
catch_clauses 
	: specific_catch_clauses general_catch_clause?
	| specific_catch_clauses? general_catch_clause
	;
*/
catch_clauses 
  : specific_catch_clauses general_catch_clause?
  | general_catch_clause
  ;
specific_catch_clauses 
	: specific_catch_clause ( specific_catch_clause )*
	;
specific_catch_clause 
	: CATCH OPEN_PARENS class_type identifier? CLOSE_PARENS block
	;
general_catch_clause 
	: CATCH block
	;
finally_clause 
	: FINALLY block
	;
checked_statement 
	: CHECKED block
	;
unchecked_statement 
	: UNCHECKED block
	;
lock_statement 
	: LOCK OPEN_PARENS expression CLOSE_PARENS embedded_statement
	;
using_statement 
	: USING OPEN_PARENS resource_acquisition CLOSE_PARENS embedded_statement
	;
/*
resource_acquisition 
	: local_variable_declaration
	| expression
	;
*/
resource_acquisition 
	: local_variable_declaration
	| expression
	;
yield_statement 
	: yield_contextual_keyword RETURN expression SEMICOLON
	| yield_contextual_keyword BREAK SEMICOLON
	;

// not used anymore; we use namespace_member_declaration+ directly

//B.2.6 Namespaces;
// entry point/ start rule
compilation_unit 
  : BYTE_ORDER_MARK? extern_alias_directives? using_directives?
    global_attribute_section*
    namespace_member_declarations? EOF
  ;
namespace_declaration 
	: NAMESPACE qi=qualified_identifier namespace_body SEMICOLON?
	;
qualified_identifier 
	: identifier ( DOT  identifier )*
	;
namespace_body 
	: OPEN_BRACE extern_alias_directives? using_directives? namespace_member_declarations? CLOSE_BRACE
	;
extern_alias_directives 
	: extern_alias_directive+
	;
extern_alias_directive 
	: EXTERN alias_contextual_keyword identifier SEMICOLON
	;
using_directives 
	: using_directive+
	;
using_directive 
	: using_alias_directive
	| using_namespace_directive
	;
using_alias_directive 
	: USING identifier ASSIGNMENT namespace_or_type_name SEMICOLON
	;
using_namespace_directive 
	: USING namespace_name SEMICOLON
	;
namespace_member_declarations 
	: namespace_member_declaration+
	;
namespace_member_declaration 
	: namespace_declaration
	| type_declaration
	;
/*
type_declaration 
	: class_declaration
	| struct_declaration
	| interface_declaration
	| enum_declaration
	| delegate_declaration
	;
*/
type_declaration 
  : attributes? all_member_modifiers?
    ( class_definition
    | struct_definition
    | interface_definition
    | enum_definition
    | delegate_definition
    )
  ;
/** starts with identifier DOUBLE_COLON */
qualified_alias_member 
	: identifier DOUBLE_COLON identifier type_argument_list_opt
	;

//B.2.7 Classes;
// not used anymore
class_declaration 
	: attributes? class_modifiers? partial_contextual_keyword? CLASS identifier type_parameter_list?
	    class_base? type_parameter_constraints_clauses? class_body SEMICOLON?
	;
class_modifiers 
	: class_modifier ( class_modifier )*
	;
class_modifier 
	: NEW
	| PUBLIC
	| PROTECTED
	| INTERNAL
	| PRIVATE
	| ABSTRACT
	| SEALED
	| STATIC
	| class_modifier_unsafe
	;
type_parameter_list 
	: LT type_parameters GT
	;
type_parameters 
	: attributes? type_parameter ( COMMA  attributes?  type_parameter )*
	;
/** identifier */
type_parameter 
	: identifier
	;
/*
class_base 
	: COLON class_type
	| COLON interface_type_list
	| COLON class_type COMMA interface_type_list
	;
*/
// class_type includes interface_type
class_base 
  : COLON class_type ( COMMA  interface_type )*
  ;
interface_type_list 
	: interface_type ( COMMA  interface_type )*
	;
type_parameter_constraints_clauses 
	: type_parameter_constraints_clause ( type_parameter_constraints_clause )*
	;
type_parameter_constraints_clause 
	: where_contextual_keyword type_parameter COLON type_parameter_constraints
	;
/*
type_parameter_constraints 
	: primary_constraint
	| secondary_constraints
	| constructor_constraint
	| primary_constraint COMMA secondary_constraints
	| primary_constraint COMMA constructor_constraint
	| secondary_constraints COMMA constructor_constraint
	| primary_constraint COMMA secondary_constraints COMMA constructor_constraint
	;
*/
type_parameter_constraints 
  : constructor_constraint
  | primary_constraint (COMMA secondary_constraints)? (COMMA constructor_constraint)?
  ;
primary_constraint 
	: class_type
	| CLASS
	| STRUCT
	;
/**
secondary_constraints 
	: interface_type
	| type_parameter
	| secondary_constraints COMMA interface_type
	| secondary_constraints COMMA type_parameter
	;
*/
// interface_type includes type_parameter
secondary_constraints
  : interface_type (COMMA interface_type)*
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
  : attributes? all_member_modifiers?
	  ( common_member_declaration
	  | TILDE identifier OPEN_PARENS CLOSE_PARENS destructor_body
	  )
  | Pp_directive
  ;
// added by chw
// combines all available modifiers
all_member_modifiers
  : (m=all_member_modifier)+
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
  | partial_contextual_keyword
  ;
/** represents the intersection of struct_member_declaration and class_member_declaration */
/*common_member_declaration
  : constant_declaration
  | field_declaration
  | method_declaration
  | property_declaration
  | event_declaration
  | indexer_declaration
  | operator_declaration
  | constructor_declaration
  | static_constructor_declaration
  | type_declaration
  ;
*/
// added by chw
common_member_declaration
  : constant_declaration2
  | typed_member_declaration
  | event_declaration2
  | conversion_operator_declarator operator_body
  // constructor_declaration and static_constructor_declaration
  | constructor_declaration2
  | type_void   method_declaration2  // we use type_void instead of VOID to switch rules
  | class_definition
  | struct_definition
  | interface_definition
  | enum_definition
  | delegate_definition
  ;
// added by chw
typed_member_declaration
  : type 
    ( interface_type DOT indexer_declaration2
    | method_declaration2
    | property_declaration2
    | indexer_declaration2
    | operator_declaration2
    | field_declaration2
    )
  ;
/*
constant_declaration 
	: attributes? constant_modifiers? CONST type constant_declarators SEMICOLON
	;
constant_modifiers 
	: constant_modifier+
	;
constant_modifier 
	: NEW
	| PUBLIC
	| PROTECTED
	| INTERNAL
	| PRIVATE
	;
*/
constant_declarators 
	: constant_declarator ( COMMA  constant_declarator )*
	;
constant_declarator 
	: identifier ASSIGNMENT constant_expression
	;
/* not used anymore;
field_declaration 
	: attributes? field_modifiers? type variable_declarators SEMICOLON
	;
field_modifiers 
	: field_modifier ( field_modifier )*
	;
field_modifier 
	: NEW
	| PUBLIC
	| PROTECTED
	| INTERNAL
	| PRIVATE
	| STATIC
	| READONLY
	| VOLATILE
	| field_modifier_unsafe
	;
*/
/** starts with identifier */
variable_declarators
	: variable_declarator ( COMMA  variable_declarator )*
	;
variable_declarator 
	: identifier
	| identifier ASSIGNMENT variable_initializer
	;
variable_initializer 
	: expression
	| array_initializer
	;
method_declaration 
	: method_header method_body
	;
method_header 
	: attributes? method_modifiers? partial_contextual_keyword? return_type member_name type_parameter_list? OPEN_PARENS formal_parameter_list? CLOSE_PARENS type_parameter_constraints_clauses?
	;
method_modifiers 
	: method_modifier+
	;
method_modifier 
	: NEW
	| PUBLIC
	| PROTECTED
	| INTERNAL
	| PRIVATE
	| STATIC
	| VIRTUAL
	| SEALED
	| OVERRIDE
	| ABSTRACT
	| EXTERN
	| method_modifier_unsafe
	;
/** type | VOID */
return_type 
	: type
	| VOID
	;
/*
member_name 
	: identifier
	| interface_type DOT identifier
	;
*/
/** interface_type */
member_name 
  : interface_type
  ;
method_body 
	: block
	| SEMICOLON
	;
/*
formal_parameter_list 
	: fixed_parameters
	| fixed_parameters COMMA parameter_array
	| parameter_array
	;
*/
formal_parameter_list 
  : parameter_array
  | fixed_parameters (COMMA parameter_array)?
  ;
fixed_parameters 
	: fixed_parameter ( COMMA fixed_parameter )*
	;
/*
fixed_parameter 
  : attributes? parameter_modifier? type identifier default_argument?
  ;
*/
// TODO add | '__arglist' etc.
fixed_parameter
  : attributes? parameter_modifier? type identifier default_argument?
  | arglist
  ;
default_argument 
	: ASSIGNMENT expression
	;
parameter_modifier 
	: REF
	| OUT
	| THIS
	;
parameter_array 
	: attributes? PARAMS array_type identifier
	;
property_declaration 
	: attributes? property_modifiers? type member_name OPEN_BRACE accessor_declarations CLOSE_BRACE
	;
property_modifiers 
	: property_modifier+
	;
property_modifier 
	: NEW
	| PUBLIC
	| PROTECTED
	| INTERNAL
	| PRIVATE
	| STATIC
	| VIRTUAL
	| SEALED
	| OVERRIDE
	| ABSTRACT
	| EXTERN
	| property_modifier_unsafe
	;
/*
accessor_declarations 
	: get_accessor_declaration set_accessor_declaration?
	| set_accessor_declaration get_accessor_declaration?
	;
*/
accessor_declarations 
  : attrs=attributes?
    mods=accessor_modifier? 
    ( get_contextual_keyword accessor_body set_accessor_declaration?
    | set_contextual_keyword accessor_body get_accessor_declaration?
    )
  ;
get_accessor_declaration 
	: attributes? accessor_modifier? get_contextual_keyword accessor_body
	;
set_accessor_declaration 
	: attributes? accessor_modifier? set_contextual_keyword accessor_body
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
	| SEMICOLON
	;
/*
event_declaration 
	: attributes? event_modifiers? EVENT type variable_declarators SEMICOLON
	| attributes? event_modifiers? EVENT type member_name OPEN_BRACE event_accessor_declarations CLOSE_BRACE
	;
*/
event_declaration 
  : attributes? event_modifiers? EVENT type
    ( variable_declarators SEMICOLON
    | member_name OPEN_BRACE event_accessor_declarations CLOSE_BRACE
    )
  ;
event_modifiers 
	: event_modifier ( event_modifier )*
	;
event_modifier 
	: NEW
	| PUBLIC
	| PROTECTED
	| INTERNAL
	| PRIVATE
	| STATIC
	| VIRTUAL
	| SEALED
	| OVERRIDE
	| ABSTRACT
	| EXTERN
	| event_modifier_unsafe
	;
event_accessor_declarations 
	: attributes?
	  ( add_contextual_keyword block remove_accessor_declaration
	  | remove_contextual_keyword block add_accessor_declaration
	  )
	;
add_accessor_declaration 
	: attributes? add_contextual_keyword block
	;
remove_accessor_declaration 
	: attributes? remove_contextual_keyword block
	;
indexer_declaration 
	: attributes? indexer_modifiers? indexer_declarator OPEN_BRACE accessor_declarations CLOSE_BRACE
	;
indexer_modifiers 
	: indexer_modifier ( indexer_modifier )*
	;
indexer_modifier 
	: NEW
	| PUBLIC
	| PROTECTED
	| INTERNAL
	| PRIVATE
	| VIRTUAL
	| SEALED
	| OVERRIDE
	| ABSTRACT
	| EXTERN
	| indexer_modifier_unsafe
	;
/*
indexer_declarator 
	: type THIS OPEN_BRACKET formal_parameter_list CLOSE_BRACKET
	| type interface_type DOT THIS OPEN_BRACKET formal_parameter_list CLOSE_BRACKET
	;
*/
indexer_declarator 
  : type (interface_type DOT)? THIS OPEN_BRACKET formal_parameter_list CLOSE_BRACKET
  ;
operator_declaration 
	: attributes? operator_modifiers operator_declarator operator_body
	;
operator_modifiers 
	: operator_modifier ( operator_modifier )*
	;
operator_modifier 
	: PUBLIC
	| STATIC
	| EXTERN
	| operator_modifier_unsafe
	;
/*
operator_declarator 
	: unary_operator_declarator
	| binary_operator_declarator
	| conversion_operator_declarator
	;
*/
operator_declarator 
  : unary_operator_declarator
  | binary_operator_declarator
  | conversion_operator_declarator
  ;
unary_operator_declarator 
	: type OPERATOR overloadable_unary_operator OPEN_PARENS type identifier CLOSE_PARENS
	;
overloadable_unary_operator 
	: PLUS
	| MINUS
	| BANG
	| TILDE
	| OP_INC
	| OP_DEC
	| TRUE
	| FALSE
	;
binary_operator_declarator 
	: type OPERATOR overloadable_binary_operator OPEN_PARENS type identifier COMMA type identifier CLOSE_PARENS
	;
overloadable_binary_operator 
	: PLUS
	| MINUS
	| STAR
	| DIV
	| PERCENT
	| AMP
	| BITWISE_OR
	| CARET
	| OP_LEFT_SHIFT
	| right_shift
	| OP_EQ
	| OP_NE
	| GT
	| LT
	| OP_GE
	| OP_LE
	;
// added by chw
/** includes the unary and the binary operators */
overloadable_operator
  : PLUS
  | MINUS
  | BANG
  | TILDE
  | OP_INC
  | OP_DEC
  | TRUE
  | FALSE
  | STAR
  | DIV
  | PERCENT
  | AMP
  | BITWISE_OR
  | CARET
  | OP_LEFT_SHIFT
  | right_shift
  | OP_EQ
  | OP_NE
  | GT
  | LT
  | OP_GE
  | OP_LE
  ;
/** starts with IMPLICIT or EXPLICIT */
conversion_operator_declarator
	: IMPLICIT OPERATOR type OPEN_PARENS type identifier CLOSE_PARENS
	| EXPLICIT OPERATOR type OPEN_PARENS type identifier CLOSE_PARENS
	;
operator_body 
	: block
	| SEMICOLON
	;
constructor_declaration 
	: attributes? constructor_modifiers? constructor_declarator constructor_body
	;
constructor_modifiers 
	: constructor_modifier+
	;
constructor_modifier 
	: PUBLIC
	| PROTECTED
	| INTERNAL
	| PRIVATE
	| EXTERN
	| constructor_modifier_unsafe
	;
constructor_declarator 
	: identifier OPEN_PARENS formal_parameter_list? CLOSE_PARENS constructor_initializer?
	;
constructor_initializer 
	: COLON BASE OPEN_PARENS argument_list? CLOSE_PARENS
	| COLON THIS OPEN_PARENS argument_list? CLOSE_PARENS
	;
constructor_body 
	: block
	| SEMICOLON
	;
static_constructor_declaration 
	: attributes? static_constructor_modifiers identifier OPEN_PARENS CLOSE_PARENS static_constructor_body
	;
/*
static_constructor_modifiers 
	: EXTERN? STATIC
	| STATIC EXTERN?
	| static_constructor_modifiers_unsafe
	;
*/
static_constructor_modifiers 
  : static_constructor_modifiers_unsafe
  ;
static_constructor_body 
	: block
	| SEMICOLON
	;
/*
destructor_declaration 
	: attributes? EXTERN? TILDE identifier OPEN_PARENS CLOSE_PARENS destructor_body
	| destructor_declaration_unsafe
	;
*/
destructor_declaration 
	: destructor_declaration_unsafe
	;
destructor_body 
	: block
	| SEMICOLON
	;
// added by chw
body
  : block
  | SEMICOLON
  ;

//B.2.8 Structs
struct_declaration 
	: attributes? struct_modifiers? partial_contextual_keyword? STRUCT identifier type_parameter_list? struct_interfaces? type_parameter_constraints_clauses? struct_body SEMICOLON?
	;
struct_modifiers 
	: struct_modifier ( struct_modifier )*
	;
struct_modifier 
	: NEW
	| PUBLIC
	| PROTECTED
	| INTERNAL
	| PRIVATE
	| struct_modifier_unsafe
	;
struct_interfaces 
	: COLON interface_type_list
	;
struct_body 
	: OPEN_BRACE struct_member_declarations? CLOSE_BRACE
	;
struct_member_declarations 
	: struct_member_declaration ( struct_member_declaration )*
	;
/*
struct_member_declaration 
	: constant_declaration
	| field_declaration
	| method_declaration
	| property_declaration
	| event_declaration
	| indexer_declaration
	| operator_declaration
	| constructor_declaration
	| static_constructor_declaration
	| type_declaration
	| struct_member_declaration_unsafe
	;
*/
struct_member_declaration 
	: attributes? all_member_modifiers?
		( common_member_declaration
		| FIXED buffer_element_type fixed_size_buffer_declarators SEMICOLON
		)
	;
//B.2.9 Arrays
/*
array_type 
	: non_array_type rank_specifiers
	;
*/
/** non_array_type rank_specifiers */
array_type 
	: base_type ((STAR | INTERR)* rank_specifier)+
	;
/*
non_array_type 
	: type
	;
*/
/** type */
non_array_type 
	: base_type (rank_specifier | INTERR | STAR)*
	;
/*
rank_specifiers 
	: rank_specifier ( rank_specifier )*
	;
*/
/** starts with OPEN_BRACKET */
rank_specifiers 
  : rank_specifier+
  ;
/** OPEN_BRACKET dim_separators? CLOSE_BRACKET */
rank_specifier 
	: OPEN_BRACKET dim_separators? CLOSE_BRACKET
	;
dim_separators 
	: COMMA ( COMMA )*
	;
/*
array_initializer 
	: OPEN_BRACE variable_initializer_list? CLOSE_BRACE
	| OPEN_BRACE variable_initializer_list COMMA CLOSE_BRACE
	;
*/
/** starts with OPEN_BRACE */
array_initializer 
  : OPEN_BRACE CLOSE_BRACE
  | OPEN_BRACE variable_initializer_list COMMA? CLOSE_BRACE
  ;
variable_initializer_list 
	: variable_initializer ( COMMA  variable_initializer )*
	;
//B.2.10 Interfaces
interface_declaration 
	: attributes? interface_modifiers? partial_contextual_keyword? INTERFACE identifier variant_type_parameter_list? interface_base? type_parameter_constraints_clauses? interface_body SEMICOLON?
	;
interface_modifiers 
	: interface_modifier ( interface_modifier )*
	;
interface_modifier 
	: NEW
	| PUBLIC
	| PROTECTED
	| INTERNAL
	| PRIVATE
	| interface_modifier_unsafe
	;
variant_type_parameter_list 
	: LT variant_type_parameters GT
	;
variant_type_parameters 
	: attributes? variance_annotation? type_parameter ( COMMA  attributes?  variance_annotation?  type_parameter )*
	;
variance_annotation 
	: IN
	| OUT
	;
interface_base 
	: COLON interface_type_list
	;
interface_body 
	: OPEN_BRACE interface_member_declarations? CLOSE_BRACE
	;
interface_member_declarations 
	: interface_member_declaration+
	;
/*
interface_member_declaration 
	: interface_method_declaration
	| interface_property_declaration
	| interface_event_declaration
	| interface_indexer_declaration
	;
*/
interface_member_declaration 
  : attributes? NEW?
    ( type
      ( identifier type_parameter_list? OPEN_PARENS formal_parameter_list? CLOSE_PARENS type_parameter_constraints_clauses? SEMICOLON
      | identifier OPEN_BRACE interface_accessors CLOSE_BRACE
      | THIS OPEN_BRACKET formal_parameter_list CLOSE_BRACKET OPEN_BRACE interface_accessors CLOSE_BRACE
      )
    | VOID identifier type_parameter_list? OPEN_PARENS formal_parameter_list? CLOSE_PARENS type_parameter_constraints_clauses? SEMICOLON
    | EVENT type identifier SEMICOLON
    )
  ;
interface_method_declaration 
	: attributes? NEW? return_type identifier type_parameter_list? OPEN_PARENS formal_parameter_list? CLOSE_PARENS type_parameter_constraints_clauses? SEMICOLON
	;
interface_property_declaration 
	: attributes? NEW? type identifier OPEN_BRACE interface_accessors CLOSE_BRACE
	;
/*
interface_accessors 
	: attributes? get_contextual_keyword SEMICOLON
	| attributes? set_contextual_keyword SEMICOLON
	| attributes? get_contextual_keyword SEMICOLON attributes? set_contextual_keyword SEMICOLON
	| attributes? set_contextual_keyword SEMICOLON attributes? get_contextual_keyword SEMICOLON
	;
*/
interface_accessors 
  : attributes?
    ( get_contextual_keyword SEMICOLON (attributes? set_contextual_keyword SEMICOLON)?
    | set_contextual_keyword SEMICOLON (attributes? get_contextual_keyword SEMICOLON)?
    )
  ;
interface_event_declaration 
	: attributes? NEW? EVENT type identifier SEMICOLON
	;
interface_indexer_declaration 
	: attributes? NEW? type THIS OPEN_BRACKET formal_parameter_list CLOSE_BRACKET OPEN_BRACE interface_accessors CLOSE_BRACE
	;


//B.2.11 Enums
enum_declaration 
	: attributes? enum_modifiers? ENUM identifier enum_base? enum_body SEMICOLON?
	;
enum_base 
	: COLON integral_type
	;
/*
enum_body 
	: OPEN_BRACE enum_member_declarations? CLOSE_BRACE
	| OPEN_BRACE enum_member_declarations COMMA CLOSE_BRACE
	;
*/
enum_body 
  : OPEN_BRACE CLOSE_BRACE
  | OPEN_BRACE enum_member_declarations COMMA? CLOSE_BRACE
  ;
enum_modifiers 
	: enum_modifier+
	;
enum_modifier 
	: NEW
	| PUBLIC
	| PROTECTED
	| INTERNAL
	| PRIVATE
	;
enum_member_declarations 
	: enum_member_declaration ( COMMA  enum_member_declaration )*
	;
/*
enum_member_declaration 
	: attributes? identifier
	| attributes? identifier ASSIGNMENT constant_expression
	;
*/
enum_member_declaration 
  : attributes? identifier (ASSIGNMENT constant_expression)?
  ;

//B.2.12 Delegates
delegate_declaration 
	: attributes? delegate_modifiers? DELEGATE return_type identifier variant_type_parameter_list? 
	    OPEN_PARENS formal_parameter_list? CLOSE_PARENS type_parameter_constraints_clauses? SEMICOLON
	;
delegate_modifiers 
	: delegate_modifier ( delegate_modifier )*
	;
delegate_modifier 
	: NEW
	| PUBLIC
	| PROTECTED
	| INTERNAL
	| PRIVATE
	| delegate_modifier_unsafe
	;


//B.2.13 Attributes
// not used anymore; we use global_attribute_section+ directly
global_attributes 
	: global_attribute_sections
	;
global_attribute_sections 
	: global_attribute_section+
	;
/*
global_attribute_section 
	: OPEN_BRACKET global_attribute_target_specifier attribute_list CLOSE_BRACKET
	| OPEN_BRACKET global_attribute_target_specifier attribute_list COMMA CLOSE_BRACKET
	;
*/
global_attribute_section 
  : OPEN_BRACKET global_attribute_target_specifier attribute_list COMMA? CLOSE_BRACKET
  ;
global_attribute_target_specifier 
	: global_attribute_target COLON
	;
global_attribute_target 
  : keyword
  | identifier
  ;
/*
global_attribute_target 
	: ASSEMBLY
	| MODULE
	;
*/
attributes 
	: attribute_sections
	;
attribute_sections 
	: attribute_section+
	;
/*
attribute_section 
	: OPEN_BRACKET attribute_target_specifier? attribute_list CLOSE_BRACKET
	| OPEN_BRACKET attribute_target_specifier? attribute_list COMMA CLOSE_BRACKET
	;
*/
attribute_section 
  : OPEN_BRACKET attribute_target_specifier? attribute_list COMMA? CLOSE_BRACKET
  ;
attribute_target_specifier 
	: attribute_target COLON
	;
attribute_target 
  : keyword
  | identifier
  ;
/*
attribute_target 
	: FIELD
	| EVENT
	| METHOD
	| PARAM
	| PROPERTY
	| RETURN
	| TYPE
	;
*/
attribute_list 
	: attribute ( COMMA  attribute )*
	;
attribute 
	: attribute_name attribute_arguments?
	;
attribute_name 
	: type_name
	;
/*
attribute_arguments 
	: OPEN_PARENS positional_argument_list? CLOSE_PARENS
	| OPEN_PARENS positional_argument_list COMMA named_argument_list CLOSE_PARENS
	| OPEN_PARENS named_argument_list CLOSE_PARENS
	;
*/
/* positional_argument_list includes named_argument_list */ 
attribute_arguments 
  : OPEN_PARENS positional_argument_list? CLOSE_PARENS
  ;
positional_argument_list 
	: arg=positional_argument
	    ( COMMA  arg2=positional_argument )*
	;
/** expression */
positional_argument 
	: attribute_argument_expression
	;
/** starts with "identifier ASSIGNMENT expression" */
named_argument_list 
	: named_argument ( COMMA  named_argument )*
	;
/** identifier ASSIGNMENT expression */
named_argument 
	: identifier ASSIGNMENT attribute_argument_expression
	;
attribute_argument_expression 
	: expression
	;


//B.3 Grammar extensions for unsafe code
class_modifier_unsafe 
	: UNSAFE
	;
struct_modifier_unsafe 
	: UNSAFE
	;
interface_modifier_unsafe 
	: UNSAFE
	;
delegate_modifier_unsafe 
	: UNSAFE
	;
field_modifier_unsafe 
	: UNSAFE
	;
method_modifier_unsafe 
	: UNSAFE
	;
property_modifier_unsafe 
	: UNSAFE
	;
event_modifier_unsafe 
	: UNSAFE
	;
indexer_modifier_unsafe 
	: UNSAFE
	;
operator_modifier_unsafe 
	: UNSAFE
	;
constructor_modifier_unsafe 
	: UNSAFE
	;
/*
destructor_declaration_unsafe 
	: attributes? EXTERN? UNSAFE? TILDE identifier OPEN_PARENS CLOSE_PARENS destructor_body
	| attributes? UNSAFE? EXTERN? TILDE identifier OPEN_PARENS CLOSE_PARENS destructor_body
	;
*/
destructor_declaration_unsafe 
  : attributes?
    ( EXTERN? UNSAFE?
    | UNSAFE EXTERN
    )  
    TILDE identifier OPEN_PARENS CLOSE_PARENS destructor_body
  ;
/*
static_constructor_modifiers_unsafe 
	: EXTERN? UNSAFE? STATIC
	| UNSAFE? EXTERN? STATIC
	| EXTERN? STATIC UNSAFE?
	| UNSAFE? STATIC EXTERN?
	| STATIC EXTERN? UNSAFE?
	| STATIC UNSAFE? EXTERN?
	;
*/
static_constructor_modifiers_unsafe 
  : (EXTERN | UNSAFE)? STATIC
  | EXTERN UNSAFE STATIC
  | UNSAFE EXTERN STATIC
  | EXTERN STATIC UNSAFE
  | UNSAFE STATIC EXTERN
  | STATIC (EXTERN | UNSAFE)
  | STATIC EXTERN UNSAFE
  | STATIC UNSAFE EXTERN
  ;
/** starts with UNSAFE or FIXED */
embedded_statement_unsafe 
	: unsafe_statement
	| fixed_statement
	;
unsafe_statement 
	: UNSAFE block
	;
type_unsafe 
	: pointer_type
	;
/*
pointer_type 
	: unmanaged_type STAR
	| VOID STAR
	;
*/
// for explanations, see http://www.antlr.org/wiki/display/ANTLR3/Left-Recursion+Removal+-+Print+Edition
// pointer_type 
//   : ( simple_type
// 	  | class_type
// 	  | VOID {allowAll = false;}
//   ) ( {allowAll}? rank_specifier
//     | {allowAll}? INTERR
//     | STAR {allowAll = true;}
//     )* STAR
//   ;
pointer_type
    :    simple_type (rank_specifier | INTERR)* STAR
    |    class_type (rank_specifier | INTERR)* STAR
    |    VOID STAR
    ;
unmanaged_type 
	: type
	;
/*
primary_no_array_creation_expression_unsafe 
	: pointer_member_access
	| pointer_element_access
	| sizeof_expression
	;
*/
primary_no_array_creation_expression_unsafe 
	: primary_expression
	;
/** starts with STAR or AMP */
unary_expression_unsafe 
	: pointer_indirection_expression
	| addressof_expression
	;
pointer_indirection_expression 
	: STAR unary_expression
	;
/* not used anymore; included in primary_expression
pointer_member_access 
	: primary_expression OP_PTR identifier
	;
*/
/* not used anymore; included in primary_no_array_creation_expression
pointer_element_access 
	: primary_no_array_creation_expression OPEN_BRACKET expression CLOSE_BRACKET
	;
*/
/** AMP unary_expression */
addressof_expression 
	: AMP unary_expression
	;
sizeof_expression 
	: SIZEOF OPEN_PARENS unmanaged_type CLOSE_PARENS
	;
fixed_statement 
	: FIXED OPEN_PARENS pointer_type fixed_pointer_declarators CLOSE_PARENS embedded_statement
	;
fixed_pointer_declarators 
	: fixed_pointer_declarator ( COMMA  fixed_pointer_declarator )*
	;
fixed_pointer_declarator 
	: identifier ASSIGNMENT fixed_pointer_initializer
	;
/*
fixed_pointer_initializer 
	: AMP variable_reference
	| expression
	;
*/
fixed_pointer_initializer 
  : AMP variable_reference
  | expression
  ;
struct_member_declaration_unsafe 
	: fixed_size_buffer_declaration
	;
fixed_size_buffer_declaration 
	: attributes? fixed_size_buffer_modifiers? FIXED buffer_element_type fixed_size_buffer_declarators SEMICOLON
	;
fixed_size_buffer_modifiers 
	: fixed_size_buffer_modifier+
	;
fixed_size_buffer_modifier 
	: NEW
	| PUBLIC
	| PROTECTED
	| INTERNAL
	| PRIVATE
	| UNSAFE
	;
buffer_element_type 
	: type
	;
fixed_size_buffer_declarators 
	: fixed_size_buffer_declarator+
	;
fixed_size_buffer_declarator 
	: identifier OPEN_BRACKET constant_expression CLOSE_BRACKET
	;
/** starts with STACKALLOC */
local_variable_initializer_unsafe 
	: stackalloc_initializer
	;
stackalloc_initializer 
	: STACKALLOC unmanaged_type OPEN_BRACKET expression CLOSE_BRACKET
	;

from_contextual_keyword: FROM;
let_contextual_keyword: LET;
where_contextual_keyword: WHERE;
join_contextual_keyword: JOIN;
on_contextual_keyword: ON;
equals_contextual_keyword: EQUALS;
into_contextual_keyword: INTO;
orderby_contextual_keyword: ORDERBY;
ascending_contextual_keyword: ASCENDING;
descending_contextual_keyword: DESCENDING;
select_contextual_keyword: SELECT;
group_contextual_keyword: GROUP;
by_contextual_keyword: BY;
partial_contextual_keyword: PARTIAL;
alias_contextual_keyword: ALIAS;
yield_contextual_keyword: YIELD;
get_contextual_keyword: GET;
set_contextual_keyword: SET;
add_contextual_keyword: ADD;
remove_contextual_keyword: REMOVE;
dynamic_contextual_keyword: DYNAMIC;
arglist: ARGLIST;
right_arrow
  : first=ASSIGNMENT second=GT {$first.index + 1 == $second.index}? // Nothing between the tokens?
  ;
right_shift
  : first=GT second=GT {$first.index + 1 == $second.index}? // Nothing between the tokens?
  ;
right_shift_assignment
  : first=GT second=OP_GE {$first.index + 1 == $second.index}? // Nothing between the tokens?
  ;
literal
  : boolean_literal
  | INTEGER_LITERAL
  | REAL_LITERAL
  | CHARACTER_LITERAL
  | STRING_LITERAL
  | NULL
  ;
boolean_literal
  : TRUE
  | FALSE
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
  | NULL
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
      class_body SEMICOLON?
  ;
struct_definition
  : STRUCT identifier type_parameter_list? struct_interfaces? type_parameter_constraints_clauses?
      struct_body SEMICOLON?
  ;
interface_definition
  : INTERFACE identifier variant_type_parameter_list? interface_base?
      type_parameter_constraints_clauses? interface_body SEMICOLON?
  ;
enum_definition
  : ENUM identifier enum_base? enum_body SEMICOLON?
  ;
delegate_definition
  : DELEGATE return_type identifier variant_type_parameter_list? OPEN_PARENS
      formal_parameter_list? CLOSE_PARENS type_parameter_constraints_clauses? SEMICOLON
  ;
event_declaration2
  : EVENT type 
      ( variable_declarators SEMICOLON
      | member_name OPEN_BRACE event_accessor_declarations CLOSE_BRACE
      )
  ;
field_declaration2
  : variable_declarators SEMICOLON
  ;
property_declaration2
  : member_name OPEN_BRACE accessor_declarations CLOSE_BRACE
  ;
constant_declaration2
  : CONST type constant_declarators SEMICOLON
  ;
indexer_declaration2
  : THIS OPEN_BRACKET formal_parameter_list CLOSE_BRACKET
      OPEN_BRACE accessor_declarations CLOSE_BRACE
  ;
destructor_definition
  : TILDE identifier OPEN_PARENS CLOSE_PARENS destructor_body
  ;
constructor_declaration2
  : identifier OPEN_PARENS formal_parameter_list? CLOSE_PARENS constructor_initializer? body
  ;
method_declaration2
  : method_member_name type_parameter_list? OPEN_PARENS formal_parameter_list? CLOSE_PARENS
      type_parameter_constraints_clauses? method_body
  ;

// added by chw to allow detection of type parameters for methods
method_member_name
  : method_member_name2
  ;
method_member_name2
  : ( identifier
    | identifier DOUBLE_COLON identifier
    ) (type_argument_list_opt DOT identifier)*
  ;
operator_declaration2
  : OPERATOR overloadable_operator OPEN_PARENS type identifier
         (COMMA type identifier)? CLOSE_PARENS operator_body
  ;
interface_method_declaration2
  : identifier type_parameter_list? OPEN_PARENS formal_parameter_list? CLOSE_PARENS type_parameter_constraints_clauses? SEMICOLON
  ;
interface_property_declaration2
  : identifier OPEN_BRACE interface_accessors CLOSE_BRACE
  ;
interface_event_declaration2
  : EVENT type identifier SEMICOLON
  ;
interface_indexer_declaration2
  : THIS OPEN_BRACKET formal_parameter_list CLOSE_BRACKET OPEN_BRACE interface_accessors CLOSE_BRACE
  ;
/** starts with DOT identifier */
member_access2
  : DOT identifier type_argument_list_opt
  ;
method_invocation2
  : OPEN_PARENS argument_list? CLOSE_PARENS
  ;
object_creation_expression2
  : OPEN_PARENS argument_list? CLOSE_PARENS object_or_collection_initializer?
  ;
  
  
