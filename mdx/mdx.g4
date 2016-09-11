
grammar mdx;

mdx_statement
   : (select_statement) EOF
   ;

select_statement
   : (WITH formula_specification)? SELECT axis_specification_list? FROM cube_specification (WHERE slicer_specification)? cell_props?
   ;

formula_specification
   : single_formula_specification +
   ;

single_formula_specification
   : member_specification
   | set_specification
   ;

set_specification
   : SET set_name AS (QUOTE expression QUOTE | expression)
   ;

member_specification
   : MEMBER member_name AS ((QUOTE value_expression QUOTE | value_expression) COMMA member_property_def_list?)
   ;

axis_specification_list
   : axis_specification (COMMA axis_specification)*
   ;

member_property_def_list
   : member_property_definition (COMMA member_property_definition)*
   ;

member_name
   : compound_id
   ;

member_property_definition
   : identifier EQ value_expression
   ;

set_name
   : compound_id
   ;

compound_id
   : identifier (DOT identifier)*
   ;

axis_specification
   : (NON EMPTY)? expression dim_props? ON axis_name
   ;

axis_name
   : identifier
   ;

dim_props
   : DIMENSION? PROPERTIES property_list
   ;

property_list
   : property (COMMA property)*
   ;

property
   : compound_id
   ;

cube_specification
   : cube_name
   ;

cube_name
   : compound_id
   ;

slicer_specification
   : expression
   ;

cell_props
   : CELL? PROPERTIES cell_property_list
   ;

cell_property_list
   : cell_property COMMA cell_property*
   ;

cell_property
   : mandatory_cell_property
   | provider_specific_cell_property
   ;

mandatory_cell_property
   : CELL_ORDINAL
   | VALUE
   | FORMATTED_VALUE
   ;

provider_specific_cell_property
   : identifier
   ;

expression
   : value_expression (COLON value_expression)*
   ;

value_expression
   : term5 (value_xor_expression | value_or_expression)*
   ;

value_xor_expression
   : XOR term5
   ;

value_or_expression
   : OR term5
   ;

term5
   : term4 (AND term4)*
   ;

term4
   : NOT term4
   | term3
   ;

term3
   : term2 (comp_op term2)*
   ;

term2
   : term ((CONCAT | PLUS | MINUS) term)*
   ;

term
   : factor ((SOLIDUS | ASTERISK) factor)*
   ;

factor
   : MINUS value_expression_primary
   | PLUS value_expression_primary
   | value_expression_primary
   ;

function
   : identifier LPAREN (exp_list)? RPAREN
   ;

value_expression_primary
   : value_expression_primary0 (DOT (unquoted_identifier | quoted_identifier | amp_quoted_identifier | function))*
   ;

value_expression_primary0
   : function
   | (LPAREN exp_list RPAREN)
   | (LBRACE (exp_list)? RBRACE)
   | case_expression
   | STRING
   | NUMBER
   | identifier
   ;

exp_list
   : expression (COMMA expression)*
   ;

case_expression
   : CASE (value_expression)? (when_list)? (ELSE value_expression)? END
   ;

when_list
   : when_clause (when_clause)*
   ;

when_clause
   : WHEN value_expression THEN value_expression
   ;

comp_op
   : EQ
   | NE
   | LT
   | GT
   | LE
   | GE
   ;

identifier
   : (unquoted_identifier | quoted_identifier)
   ;

unquoted_identifier
   : keyword
   | ID
   ;

amp_quoted_identifier
   : AMP_QUOTED_ID
   ;

quoted_identifier
   : QUOTED_ID
   ;

keyword
   : DIMENSION
   | PROPERTIES
   ;


QUOTE
   : '\''
   ;


ASTERISK
   : '*'
   ;


COLON
   : ':'
   ;


SEMICOLON
   : ';'
   ;


COMMA
   : ','
   ;


CONCAT
   : '||'
   ;


DOT
   : '.'
   ;


EQ
   : '='
   ;


GE
   : '>='
   ;


GT
   : '>'
   ;


LBRACE
   : '{'
   ;


LE
   : '<='
   ;


LPAREN
   : '('
   ;


LT
   : '<'
   ;


MINUS
   : '-'
   ;


NE
   : '<>'
   ;


PLUS
   : '+'
   ;


RBRACE
   : '}'
   ;


RPAREN
   : ')'
   ;


SOLIDUS
   : '/'
   ;


AND
   : 'AND'
   ;


AS
   : 'AS'
   ;


CASE
   : 'CASE'
   ;


CELL
   : 'CELL'
   ;


CELL_ORDINAL
   : 'CELL_ORDINAL'
   ;


CREATE
   : 'CREATE'
   ;


DIMENSION
   : 'DIMENSION'
   ;


ELSE
   : 'ELSE'
   ;


EMPTY
   : 'EMPTY'
   ;


END
   : 'END'
   ;


FORMATTED_VALUE
   : 'FORMATTED_VALUE'
   ;


FROM
   : 'FROM'
   ;


GLOBAL
   : 'GLOBAL'
   ;


MEMBER
   : 'MEMBER'
   ;


NON
   : 'NON'
   ;


NOT
   : 'NOT'
   ;


ON
   : 'ON'
   ;


OR
   : 'OR'
   ;


PROPERTIES
   : 'PROPERTIES'
   ;


SELECT
   : 'SELECT'
   ;


SESSION
   : 'SESSION'
   ;


SET
   : 'SET'
   ;


THEN
   : 'THEN'
   ;


VALUE
   : 'VALUE'
   ;


WHEN
   : 'WHEN'
   ;


WHERE
   : 'WHERE'
   ;


XOR
   : 'XOR'
   ;


WITH
   : 'WITH'
   ;


NUMBER
   : ('0' .. '9') +
   ;


F
   : '0' .. '9' + '.' '0' .. '9'*
   ;


ID
   : ('a' .. 'z' | 'A' .. 'Z' | '_' | '$') ('a' .. 'z' | 'A' .. 'Z' | '_' | '0' .. '9' | '$')*
   ;


AMP_QUOTED_ID
   : '[&' (ID ((' ' | '\t') + ID)* | NUMBER) ']'
   ;


QUOTED_ID
   : ('[' (ID ((' ' | '\t') + ID)* | NUMBER) ']')
   ;


STRING
   : '"' (~ '"')* '"' | '\'' (~ '\'')* '\''
   ;


WS
   : (' ' | '\t' | '\r' | '\f' | '\n') + -> skip
   ;
