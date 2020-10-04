

parser grammar PowerBuilderDWParser;

options { 
tokenVocab = PowerBuilderDWLexer; 
}

start_rule
   : header_rule? datawindow_rule + EOF
   ;

header_rule
   : EXPORT_HEADER* (RELEASE NUMBER SEMI)? 
   ;

datawindow_rule
    :datawindow_property+
    ;

datawindow_property
   : attribute_name LPAREN datawindow_property_attribute_sub* RPAREN
   | TABLE LPAREN table_attribute+ RPAREN     
   | COLUMN LPAREN datawindow_property_attribute_sub* RPAREN
   ;

table_attribute
    : column_attribute
    | retrieve_attribute
    ;



column_attribute
    : COLUMN EQ LPAREN TYPE EQ dataTypeSub LPAREN numeric_atom RPAREN  datawindow_property_attribute_sub+ RPAREN
    ;

retrieve_attribute
    : RET_LIT ARGS_LIT? SORT_LIT?    
    ;

datawindow_property_attribute_sub
   : ( NULL
     | numeric_atom
     | DQUOTED_STRING
     | DATE
     | TIME
     | attribute_name eq=EQ (attribute_value array_decl_sub? | LPAREN datawindow_property_attribute_sub+ RPAREN)
     ) COMMA?
   ;

attribute_name
   : (identifier_name | TYPE | UPDATE) NUMBER? (DOT (identifier_name | CASE | TYPE | ON | DYNAMIC))*
   ;

identifier_name
   : ID
   ;

attribute_value
   : atom_sub_call1
   | atom_sub_member1
   | MINUS? numeric_atom
   | boolean_atom
   | ENUM
   | DQUOTED_STRING
   | QUOTED_STRING
   | DATE
   | TIME
   | TYPE
   | TO
   | FROM
   | REF
   | NULL
   | OPEN
   | LPAREN LPAREN (expression | dataTypeSub) (COMMA (expression | dataTypeSub))? RPAREN (COMMA LPAREN (expression | dataTypeSub) (COMMA (expression | dataTypeSub))? RPAREN)* RPAREN
   | dataTypeSub (LPAREN NUMBER RPAREN)?
   ;

numeric_atom
   : NUMBER
   ;

dataTypeSub
   : ANY
   | BLOB
   | BOOLEAN
   | BYTE
   | CHARACTER
   | CHAR
   | DATE_TYPE
   | DATETIME
   | DECIMAL
   | DEC
   | DOUBLE
   | INTEGER
   | INT
   | LONG
   | LONGLONG
   | REAL
   | STRING
   | TIME_TYPE
   | UNSIGNEDINTEGER
   | UINT
   | UNSIGNEDLONG
   | ULONG
   | WINDOW
   ;

expression
   : close_call_sub
   | LCURLY
   ;

array_decl_sub
   : BRACES
   | LBRACE ((PLUS | MINUS)? NUMBER (TO (PLUS | MINUS)? NUMBER)? (COMMA (PLUS | MINUS)? NUMBER (TO (PLUS | MINUS)? NUMBER)?)*)? RBRACE
   ;

close_call_sub
   : CLOSE LPAREN expression_list RPAREN
   | HALT CLOSE
   ;

expression_list
   : REF? expression (COMMA REF? expression)*
   ;

atom_sub_call1
   : (identifier | DESCRIBE) LPAREN expression_list? RPAREN
   ;

identifier
   : identifier_name
   | SUPER COLONCOLON (CREATE | DESTROY | identifier_name_ex)
   | identifier_name COLONCOLON (CREATE | DESTROY)
   | identifier_name DOT (CREATE | DESTROY)
   | identifier_name COLONCOLON identifier_name_ex
   ;

identifier_name_ex
   : identifier_name
   | SELECT
   | TYPE
   | UPDATE
   | DELETE
   | OPEN
   | CLOSE
   | GOTO
   | INSERT
   | DESCRIBE
   | TIME
   | READONLY
   ;

atom_sub_member1
   : identifier
   ;

boolean_atom
   : TRUE
   | FALSE
   ;