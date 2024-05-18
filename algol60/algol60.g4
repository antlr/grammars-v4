// $antlr-format alignTrailingComments true, columnLimit 150, minEmptyLines 1, maxEmptyLinesToKeep 1, reflowComments false, useTab false
// $antlr-format allowShortRulesOnASingleLine false, allowShortBlocksOnASingleLine true, alignSemicolons hanging, alignColons hanging

grammar algol60;

options {
    caseInsensitive = true;
}

// Derived from the authoratitive source ISO 1538
// You can download a free copy at:
// http://www.softwarepreservation.org/projects/ALGOL/report/ISO1538.pdf

// The spec, typical for most grammars at the time, do not distinguish
// BNF between the lexer and parser. The rules are divided here as:
//
// TBD

// 1.1
empty_
    :
    ;

fragment Basic_symbol
    : Letter
    | Digit
    | Logical_value_f
    | Delimiter
    ;

// The alphabet can be extended in any way.
// If memory serves me, I recall no case sensitivity.
Array_
    : 'array'
    ;

Begin_
    : 'begin'
    ;

Boolean_
    : 'boolean'
    ;

Comment_
    : 'comment'
    ;

Do_
    : 'do'
    ;

Else_
    : 'else'
    ;

End_
    : 'end'
    ;

False_
    : 'false'
    ;

For_
    : 'for'
    ;

Goto_
    : 'go' Ws* 'to'
    ;

If_
    : 'if'
    ;

Integer_
    : 'integer'
    ;

Label_
    : 'label'
    ;

Own_
    : 'own'
    ;

Procedure_
    : 'procedure'
    ;

Real_
    : 'real'
    ;

Step_
    : 'step'
    ;

String_
    : 'string'
    ;

Switch_
    : 'switch'
    ;

Then_
    : 'then'
    ;

True_
    : 'true'
    ;

Until_
    : 'until'
    ;

Value_
    : 'value'
    ;

While_
    : 'while'
    ;

And_
    : '⋀'
    | '&'
    ; //  '⊃' | ∧ | '⋀'

Assign_
    : ':='
    ;

Colon_
    : ':'
    ;

Comma_
    : ','
    ;

Dot_
    : '.'
    ;

Divide_
    : '/'
    | '÷'
    ;

Eor_
    : '^='
    ;

Eq_
    : '='
    ;

Equiv_
    : '≡'
    ;

Exp_
    : '↑'
    | '^'
    ;

Gt_
    : '>'
    ;

Ge_
    : '≥'
    | '>='
    ;

Includes_
    : '⊃'
    ;

Lb_
    : '['
    ;

Le_
    : '<='
    | '≤'
    ;

LP_
    : '('
    ;

Lt_
    : '<'
    ;

Minus_
    : '-'
    | '–'
    ;

Mult_
    : '×'
    | '*'
    ;

Ne_
    : '≠'
    | '!='
    ;

Not_
    : '¬'
    | '!'
    ;

Or_
    : '⋁'
    | '|'
    ;

Plus_
    : '+'
    ;

Rb_
    : ']'
    ;

Rp_
    : ')'
    ;

Semi_
    : ';'
    ;

Underscore_
    : '_'
    ;

// 2.1
fragment Letter
    : [a-z]
    | '_' // an extension.
    ;

// 2.2.1
fragment Digit
    : '0'
    | '1'
    | '2'
    | '3'
    | '4'
    | '5'
    | '6'
    | '7'
    | '8'
    | '9'
    ;

fragment ULCorner_f options {
    caseInsensitive = false;
}
    : '\u231C'
    ;

ULCorner
    : ULCorner_f
    ;

fragment URCorner_f options {
    caseInsensitive = false;
}
    : '\u231D'
    ;

URCorner
    : URCorner_f
    ;

// 2.2.2
fragment Logical_value_f
    : True_
    | False_
    ;

Logical_value
    : Logical_value_f
    ;

// 2.3
fragment Delimiter
    : Operator
    | Separator
    | Bracket
    | Declarator
    | Specificator
    ;

fragment Operator
    : Arithmetic_operator
    | Relational_operator_f
    | Logical_operator
    | Sequential_operator
    ;

fragment Arithmetic_operator
    : Plus_
    | Minus_
    | Mult_
    | Divide_
    | Exp_
    ;

fragment Relational_operator_f
    : Lt_
    | Le_
    | Eq_
    | Ne_
    | Gt_
    | Ge_
    ;

Relational_operator
    : Relational_operator_f
    ;

fragment Logical_operator
    : Equiv_
    | Includes_
    | Or_
    | And_
    | Not_
    ;

fragment Sequential_operator
    : Goto_
    | If_
    | Then_
    | Else_
    | For_
    | Do_
    ;

fragment Separator
    : Comma_
    | Dot_
    | '10'
    | Colon_
    | Semi_
    | Assign_
    | Underscore_
    | Step_
    | Until_
    | While_
    | Comment_
    ;

fragment Bracket
    : LP_
    | Rp_
    | Lb_
    | Rb_
    | '⌈'
    | '⌝'
    | Begin_
    | End_
    ;

fragment Declarator
    : Own_
    | Boolean_
    | Integer_
    | Real_
    | Array_
    | Switch_
    | Procedure_
    ;

fragment Specificator
    : String_
    | Label_
    | Value_
    ;

// Antlr restriction cannot use ~Semi_.
Comment
    : Comment_ ~';'+ Semi_ -> channel(HIDDEN)
    ;

// 2.4.1
// rewritten to avoid MLR.
Identifier
    : (Letter | Digit)+
    ;

// 2.5.1
// rewritten to avoid MLR.
Unsigned_integer
    : Digit+
    ;

integer
    : Unsigned_integer
    | Plus_ Unsigned_integer
    | Minus_ Unsigned_integer
    ;

Decimal_fraction
    : Dot_ Unsigned_integer
    ;

// unfold
Exponential_part
    : '10' (Unsigned_integer | Plus_ Unsigned_integer | Minus_ Unsigned_integer)
    ;

Decimal_number
    : Unsigned_integer
    | Decimal_fraction
    | Unsigned_integer Decimal_fraction
    ;

Unsigned_number
    : Decimal_number
    | Exponential_part
    | Decimal_number Exponential_part
    ;

number
    : Unsigned_number
    | Plus_ Unsigned_number
    | Minus_ Unsigned_number
    ;

// 2.6.1
fragment Proper_string options {
    caseInsensitive = false;
}
    : ~('\u231C' | '\u231D')*
    ;

fragment Open_string
    : Proper_string
    | Proper_string Closed_string Open_string
    ;

fragment Closed_string
    : ULCorner Open_string URCorner
    ;

fragment StdString
    : Closed_string
    | Closed_string StdString
    ;

String
    : StdString // String according to spec.
    | '"' ~'"'* '"'
    | '`' ~'\''* '\''
    ; // Additional non-standard strings used in examples.

// 3
expression
    : arithmetic_expression
    | boolean_expression
    | designational_expression
    ;

// 3.1
variable_identifier
    : Identifier
    ;

simple_variable
    : variable_identifier
    ;

subscript_expression
    : arithmetic_expression
    ;

subscript_list
    : subscript_expression
    | subscript_list Comma_ subscript_expression
    ;

array_identifier
    : Identifier
    ;

subscripted_variable
    : array_identifier Lb_ subscript_list Rb_
    ;

variable
    : simple_variable
    | subscripted_variable
    ;

// 3.2.1
procedure_identifier
    : Identifier
    ;

// The following rules replaced because there is no context sensitive lexing.
//letter_string : letter | letter_string letter ;
//parameter_delimiter : Comma_ | Rp_ letter_string Colon_ LP_ ;
parameter_delimiter
    : Comma_
    | Rp_ Identifier Colon_ LP_
    ;

actual_parameter
    : String
    | expression
    | array_identifier
    | switch_identifier
    | procedure_identifier
    ;

actual_parameter_list
    : actual_parameter
    | actual_parameter_list parameter_delimiter actual_parameter
    ;

function_designator
    : procedure_identifier actual_parameter_part
    ;

// 3.3.1
adding_operator
    : Plus_
    | Minus_
    ;

multiplying_operator
    : Mult_
    | Divide_
    ;

primary
    : Unsigned_number
    | variable
    | function_designator
    | LP_ arithmetic_expression Rp_
    ;

factor
    : primary
    | factor Exp_ primary
    ;

term
    : factor
    | term multiplying_operator factor
    ;

simple_arithmetic_expression
    : term
    | adding_operator term
    | simple_arithmetic_expression adding_operator term
    ;

if_clause
    : If_ boolean_expression Then_
    ;

arithmetic_expression
    : simple_arithmetic_expression
    | if_clause simple_arithmetic_expression Else_ arithmetic_expression
    ;

// 3.4.1
// dup relational_operator.
relation
    : simple_arithmetic_expression Relational_operator simple_arithmetic_expression
    ;

boolean_primary
    : Logical_value
    | variable
    | function_designator
    | relation
    | LP_ boolean_expression Rp_
    ;

boolean_secondary
    : boolean_primary
    | Not_ boolean_primary
    ;

boolean_factor
    : boolean_secondary
    | boolean_factor And_ boolean_secondary
    ;

boolean_term
    : boolean_factor
    | boolean_term Or_ boolean_factor
    ;

implication
    : boolean_term
    | implication Includes_ boolean_term
    ;

simple_boolean
    : implication
    | simple_boolean Equiv_ implication
    ;

boolean_expression
    : simple_boolean
    | if_clause simple_boolean Else_ boolean_expression
    ;

// 3.5.1
label
    : Identifier
    | Unsigned_integer
    ;

switch_identifier
    : Identifier
    ;

switch_designator
    : switch_identifier Lb_ subscript_expression Rb_
    ;

simple_designational_expression
    : label
    | switch_designator
    | LP_ designational_expression Rp_
    ;

designational_expression
    : simple_designational_expression
    | if_clause simple_designational_expression Else_ designational_expression
    ;

// 4.1.1
unlabelled_basic_statement
    : assignment_statement
    | go_to_statement
    | dummy_statement
    | procedure_statement
    ;

basic_statement
    : unlabelled_basic_statement
    | label Colon_ basic_statement
    ;

unconditional_statement
    : basic_statement
    | compound_statement
    | block
    ;

statement
    : unconditional_statement
    | conditional_statement
    | for_statement
    ;

compound_tail
    : statement End_
    | statement Semi_ compound_tail
    ;

block_head
    : Begin_ declaration
    | block_head Semi_ declaration
    ;

unlabelled_compound
    : Begin_ compound_tail
    ;

unlabelled_block
    : block_head Semi_ compound_tail
    ;

compound_statement
    : unlabelled_compound
    | label Colon_ compound_statement
    ;

block
    : unlabelled_block
    | label Colon_ block
    ;

program
    : (block | compound_statement) EOF
    ;

// 4.2.1
destination
    : variable
    | procedure_identifier
    ;

left_part
    : variable Assign_
    | procedure_identifier Assign_
    ;

left_part_list
    : left_part
    | left_part_list left_part
    ;

assignment_statement
    : left_part_list arithmetic_expression
    | left_part_list boolean_expression
    ;

// 4.3.1
go_to_statement
    : Goto_ designational_expression
    ;

// 4.4.1
dummy_statement
    : empty_
    ;

// 4.5.1
// dup if_clause
// dup unconditional statement
if_statement
    : if_clause unconditional_statement
    ;

conditional_statement
    : if_statement
    | if_statement Else_ statement
    | if_clause for_statement
    | label Colon_ conditional_statement
    ;

// 4.6.1
for_list_element
    : arithmetic_expression
    | arithmetic_expression Step_ arithmetic_expression Until_ arithmetic_expression
    | arithmetic_expression While_ boolean_expression
    ;

for_list
    : for_list_element
    | for_list Comma_ for_list_element
    ;

for_clause
    : For_ variable Assign_ for_list Do_
    ;

for_statement
    : for_clause statement
    | label Colon_ for_statement
    ;

// 4.7.1
// dup actual_parameter
// dup letter_string
// dup parameter_delimiter
// dup actual_parameter_list
actual_parameter_part
    : empty_
    | LP_ actual_parameter_list Rp_
    ;

procedure_statement
    : procedure_identifier actual_parameter_part
    ;

// 4.7.8
code
    : .*
    ; // match anything.

// 5
declaration
    : type_declaration
    | array_declaration
    | switch_declaration
    | procedure_declaration
    ;

// 5.1.1
type_list
    : simple_variable
    | simple_variable Comma_ type_list
    ;

type_
    : Real_
    | Integer_
    | Boolean_
    ;

local_or_own
    : empty_
    | Own_
    ;

type_declaration
    : local_or_own type_ type_list
    ;

// 5.2.1
lower_bound
    : arithmetic_expression
    ;

upper_bound
    : arithmetic_expression
    ;

bound_pair
    : lower_bound Colon_ upper_bound
    ;

bound_pair_list
    : bound_pair
    | bound_pair_list Comma_ bound_pair
    ;

array_segment
    : array_identifier Lb_ bound_pair_list Rb_
    | array_identifier Comma_ array_segment
    ;

array_list
    : array_segment
    | array_list Comma_ array_segment
    ;

array_declarer
    : type_ Array_
    | Array_
    ;

array_declaration
    : local_or_own array_declarer array_list
    ;

// 5.3.1
switch_list
    : designational_expression
    | switch_list Comma_ designational_expression
    ;

switch_declaration
    : Switch_ switch_identifier Assign_ switch_list
    ;

// 5.4.1
formal_parameter
    : Identifier
    ;

formal_parameter_list
    : formal_parameter
    | formal_parameter_list parameter_delimiter formal_parameter
    ;

formal_parameter_part
    : empty_
    | LP_ formal_parameter_list Rp_
    ;

identifier_list
    : Identifier
    | identifier_list Comma_ Identifier
    ;

value_part
    : Value_ identifier_list Semi_
    | empty_
    ;

specifier
    : String_
    | type_
    | Array_
    | type_ Array_
    | Label_
    | Switch_
    | Procedure_
    | type_ Procedure_
    ;

specification_part
    : empty_
    | specifier identifier_list Semi_
    | specification_part specifier identifier_list
    ;

procedure_heading
    : procedure_identifier formal_parameter_part Semi_ value_part specification_part
    ;

procedure_body
    : statement
    | code
    ;

procedure_declaration
    : Procedure_ procedure_heading procedure_body
    | type_ Procedure_ procedure_heading procedure_body
    ;

WS
    : Ws -> channel(HIDDEN)
    ;

fragment Ws
    : [ \r\n\t]+
    ;