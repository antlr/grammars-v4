grammar Verilog3;

options
	{
	language= C;
	output=AST;
	ASTLabelType= pANTLR3_BASE_TREE;
	//tokenVocab = Verilog;  //call the vocabulary 'Verilog'
    //testLiterals = false;  don't automatically test for literals
    k = 3;                  // 3 characters of lookahead
	}


//-----------------------------------------------------------------------------
// Source Text
//-----------------------------------------------------------------------------

source_text :
        ( description )*
	EOF
        ;

description :
        module |
        udp |
	directive
        ;

module :
        ( 'module' | 'macromodule' )
	name_of_module
        ( list_of_ports )? SEMI
	( module_item )*
	'endmodule'
        ;

list_of_ports :
        LPAREN port ( COMMA port )* RPAREN
        ;

port :
	(port_expression)? |
        DOT name_of_port LPAREN (port_expression)? RPAREN
	;

port_expression :
        port_reference |
	LCURLY port_reference ( COMMA port_reference )* RCURLY
        ;

port_reference :
        ( name_of_variable LBRACK expression COLON ) =>
        name_of_variable LBRACK expression COLON expression RBRACK |
        ( name_of_variable LBRACK ) =>
        name_of_variable LBRACK expression RBRACK |
        name_of_variable
        ;

module_item : 
	 // ambiguity between net_declaration and continuous_assign,
	 // but parser gets it right: keyword chosen over IDENTIFIER.
        parameter_declaration |
        input_declaration |
        output_declaration |
        inout_declaration |
        //net_declaration |
        reg_declaration |
        time_declaration |
        integer_declaration |
        real_declaration |
        event_declaration |
		gate_declaration |
        instantiation |
        parameter_override |
        continuous_assign |
        specify_block |
        initial_statement |
        always_statement |
        task |
        function |
		directive
        ;

instantiation:
	(module_instantiation) => module_instantiation |
	udp_instantiation
	;

//----------------------------------------------------------------------------
// UDP specs
//----------------------------------------------------------------------------

udp :
        'primitive' name_of_UDP
	LPAREN name_of_variable ( COMMA name_of_variable )* RPAREN SEMI
        (udp_declaration)+
	(udp_initial_statement)?
	table_definition
        'endprimitive'
        ;

udp_port_list :
        udp_name_of_port ( COMMA udp_name_of_port )*
        ;

udp_declaration :
        output_declaration |
        input_declaration |
        reg_declaration
        ;

udp_initial_statement :
        'initial' output_terminal_name ASSIGN init_val SEMI
        ;

   // Use a semantic predicate to determine whether a matched NUMBER
   // is a valid special value in the given context.
   // This kludge avoids having the special values in the Literals table,
   // thus avoiding a lexical conflict.
init_val :
        '1\'b0' |
        '1\'b1' |
        '1\'bx' |
	n=NUMBER
	{ $n.text=="0" || $n.text=="1"}?
	;

table_definition :
        'table' table_entries 'endtable'
        ;

   // Don't try to parse table entries; just collect them.
   // There are ambiguities between edge_symbol and level_symbol,
   // and textbook Verilog examples don't seem to follow rules
   // completely. For example,
   //	'0    00    :    0;'
   // doesn't match grammar because of '00', but is frequently used.
table_entries :
	//(sequential_entry) => (sequential_entry)+ |
	//(combinational_entry)+
	(( ~(SEMI | 'endtable') )+ SEMI)*
	;


/********
 ******** Start of commented out rules related to table entries.
 ********

combinational_entry :
	level_input_list COLON output_symbol SEMI
	;

sequential_entry :
	input_list COLON state COLON next_state SEMI
	;

input_list :
	(level_input_list) => level_input_list |
	edge_input_list
	;

level_input_list :
	(level_symbol)+
	;

edge_input_list :
	(level_symbol)* edge (level_symbol)*
	;

edge :
	LPAREN level_symbol level_symbol RPAREN |
	edge_symbol
	;

state :
	level_symbol
	;

next_state :
	output_symbol |
	MINUS
	;

   // Next 3 rules use semantic predicates to determine whether a matched
   // NUMBER or IDENTIFIER is a valid special value in the given context.
   // This kludge avoids having the special values in the Literals table,
   // thus avoiding a lexical conflict.
output_symbol:
	n=NUMBER
	{ $n.text=="0" || $n.text=="1"}?
      |
	i=IDENTIFIER
	{ $i.text=="x" || $i.text=="X"}?
	;

level_symbol:
	QUESTION
      |
	n=NUMBER
	{ $n.text=="0" || $n.text=="1"}?
      |
	i=IDENTIFIER
	{ $i.text=="x" || $i.text=="X" ||
	  $i.text=="b" || $i.text=="B" }?
	;

edge_symbol:
	STAR
      |
	i=IDENTIFIER
	{ $i.text=="r" || $i.text=="R" ||
	  $i.text=="f" || $i.text=="F" ||
	  $i.text=="p" || $i.text=="P" ||
	  $i.text=="n" || $i.text=="N" }?
	;

 ********
 ******** End of commented out rules related to table entries.
 ********/


task :
        'task' name_of_task SEMI
        (tf_declaration)*
        statement_or_null
        'endtask'
        ;

function :
        'function' (range_or_type)? name_of_function SEMI
        (tf_declaration)+
        statement
        'endfunction'
        ;

range_or_type :
        range |
        'integer' |
        'real'
        ;

tf_declaration :
        parameter_declaration |
        output_declaration |
        input_declaration |
        inout_declaration |
        reg_declaration |
        time_declaration |
        integer_declaration |
        real_declaration |
        event_declaration 
        ;


//----------------------------------------------------------------------------
// Declarations
//----------------------------------------------------------------------------

parameter_declaration :
        'parameter' (range)? list_of_param_assignments SEMI
        ;

list_of_param_assignments :
        param_assignment ( COMMA param_assignment )*
        ;

param_assignment :
        identifier ASSIGN expression
        ;

input_declaration :
        'input' (range)? list_of_variables SEMI
        ;

output_declaration :
        'output' (range)? list_of_variables SEMI
        ;

inout_declaration :
        'inout' (range)? list_of_variables SEMI
        ;

net_declaration :
        ( net_type (expandrange)? ) =>
        net_type (expandrange)? (delay)?
           list_of_assigned_variables SEMI |
        KW_TRIREG (charge_strength)? (expandrange)? (delay)?
           list_of_variables SEMI
        ;

net_type :
        'wire' | 
        'tri' |
        'tri1' |
        'supply0' |
        'wand' |
        'triand' |
        'tri0' |
        'supply1' |
        'wor' |
        'trior' |
	    KW_TRIREG
        ;

expandrange :
        'scalared' range |
	'vectored' range |
	range
        ;

reg_declaration :
        'reg' (range)? list_of_register_variables SEMI
        ;

time_declaration :
        'time' list_of_register_variables SEMI
        ;

integer_declaration :
        'integer' list_of_register_variables SEMI
        ;

real_declaration :
        'real' list_of_variables SEMI
        ;

event_declaration :
        'event' name_of_event ( COMMA name_of_event )* SEMI
        ;

continuous_assign :
        KW_ASSIGN (drive_strength)? (delay)? list_of_assignments SEMI |
        net_type (drive_strength)? (expandrange)? (delay)?
           list_of_assignments SEMI
        ;

parameter_override :
        'defparam' list_of_param_assignments SEMI
        ;

list_of_variables :
        name_of_variable ( COMMA name_of_variable )*
        ;

list_of_assigned_variables :
        name_of_variable ( ASSIGN expression )?
	( COMMA name_of_variable ( ASSIGN expression )? )*
        ;

list_of_register_variables :
        register_variable ( COMMA register_variable )*
        ;

register_variable :
        name_of_register |
        name_of_memory LBRACK expression COLON expression RBRACK
        ;

charge_strength :
        LPAREN 'small'  RPAREN |
        LPAREN 'medium' RPAREN |
        LPAREN 'large'  RPAREN
        ;

drive_strength :
        LPAREN strength0 COMMA strength1 RPAREN |
        LPAREN strength1 COMMA strength0 RPAREN
        ;

strength0 :
        'supply0' |
        'strong0' |
        'pull0' |
        'weak0' |
	'highz0'
        ;

strength1 :
        'supply1' |
        'strong1' |
        'pull1' |
        'weak1' |
	'highz1'
        ;

range :
	(LBRACK expression COLON) =>
        LBRACK expression COLON expression RBRACK |
        LBRACK expression RBRACK
        ;

list_of_assignments :
        assignment ( COMMA assignment )*
        ;


//----------------------------------------------------------------------------
// Primitive Instances
//----------------------------------------------------------------------------

gate_declaration :
	gate_type (drive_strength)? (delay)?
          gate_instance ( COMMA gate_instance )* SEMI
	;

gate_type :
        'and' |
        'nand' |
        'or' |
        'nor' |
        'xor' |
        'xnor' |
        'buf' |
        'bufif0' |
        'bufif1' |
        'not' |
        'notif0' |
        'notif1' |
        'pulldown' |
        'pullup' |
        'nmos' |
        'rnmos' |
        'pmos' |
        'rpmos' |
        'cmos' |
        'rcmos' |
        'tran' |
        'rtran' |
        'tranif0' |
        'rtranif0' |
        'tranif1' |
        'rtranif1'
        ;

delay :
        POUND NUMBER | 
	POUND identifier |
        POUND LPAREN mintypmax_expression
	        ( COMMA mintypmax_expression
		  ( COMMA mintypmax_expression )?
		)?
	      RPAREN
        ;

gate_instance :
        (name_of_gate_instance)?
	LPAREN terminal ( COMMA terminal )* RPAREN
        ;

udp_instantiation :
	name_of_UDP (drive_strength)? (delay)?
          udp_instance ( COMMA udp_instance )* SEMI
	;

udp_instance :
        (name_of_UDP_instance)?
	LPAREN terminal ( COMMA terminal )* RPAREN
        ;

terminal :
        expression
   // | IDENTIFIER
        ;

//----------------------------------------------------------------------------
// Module Instantiations
//----------------------------------------------------------------------------

module_instantiation :
        name_of_module (parameter_value_assignment)?
        module_instance ( COMMA module_instance )* SEMI
        ;

parameter_value_assignment :
        POUND LPAREN expression ( COMMA expression )* RPAREN
        ;

module_instance :
        name_of_instance LPAREN list_of_module_connections RPAREN
        ;

list_of_module_connections :
        module_port_connection ( COMMA module_port_connection )* |
        named_port_connection ( COMMA named_port_connection )*
        ;

module_port_connection :
        expression |
	// NULL
        ;

	// expression below isn't optional according to Palnitkar, but
	// several examples generated by Cadence use this syntax.
named_port_connection :
        DOT IDENTIFIER LPAREN (expression)? RPAREN
        ;

//----------------------------------------------------------------------------
// Behavioral Statements
//----------------------------------------------------------------------------

initial_statement :
        'initial' statement
        ;

always_statement :
        'always' statement
        ;

statement_or_null :
        (statement) => statement |
	SEMI
        ;

statement :
        (lvalue ASSIGN) => blocking_assignment SEMI |
        (lvalue LE) => non_blocking_assignment SEMI |
        conditional_statement |
        case_statement |
        loop_statement |
        procedural_timing_control_statement |
        wait_statement |
        event_trigger |
        seq_block |
        par_block |
        task_enable |
        system_task_enable |
        disable_statement |
        procedural_continuous_assignment
        ;

assignment :
        lvalue ASSIGN expression
        ;

blocking_assignment :
        lvalue ASSIGN ( delay_or_event_control )?  expression
        ;

non_blocking_assignment :
        lvalue LE ( delay_or_event_control )?  expression
        ;

	  // 'else' clause is inherently ambiguous; ANTLR gets it right,
	  // so suppress warning.
conditional_statement :
        'if' LPAREN expression RPAREN statement_or_null
        ( : 'else' statement_or_null)?
        ;

case_statement :
        case_keyword LPAREN expression RPAREN (case_item)+ 'endcase'
        ;

case_keyword :
        'case' | 'casez' | 'casex'
        ;

case_item :
        expression ( COMMA expression )* COLON statement_or_null |
        'default' (COLON)? statement_or_null
        ;

loop_statement :
        'forever' statement |
        'repeat' LPAREN expression RPAREN statement |
        'while' LPAREN expression RPAREN statement |
        'for' LPAREN assignment SEMI expression SEMI assignment RPAREN statement
        ;

procedural_timing_control_statement :
        delay_or_event_control statement_or_null
        ;

wait_statement :
        'wait' LPAREN expression RPAREN statement_or_null
        ;

event_trigger :
        TRIGGER name_of_event SEMI
        ;

disable_statement :
        'disable' IDENTIFIER SEMI
        ;

seq_block :
        'begin'
        ( COLON name_of_block (block_declaration)* )?
        (statement)*
	'end'
	;

par_block :
        'fork'
        ( COLON name_of_block (block_declaration)* )?
        (statement)*
	'join'
        ;

block_declaration :
        parameter_declaration |
        reg_declaration |
        integer_declaration |
        real_declaration |
        time_declaration |
        event_declaration 
        ;

task_enable :
        name_of_task ( LPAREN expression (COMMA (expression)?)* RPAREN )?
	SEMI
        ;

system_task_enable :
        SYSTEM_TASK_NAME ( LPAREN expression (COMMA (expression)?)* RPAREN )?
	SEMI
        ;

procedural_continuous_assignment :
        KW_ASSIGN assignment SEMI |
        'deassign' lvalue SEMI |
        'force' assignment SEMI |
        'release' lvalue SEMI
        ;

delay_or_event_control :
        delay_control |
        event_control
        ;

//----------------------------------------------------------------------------
// Specify Section
//----------------------------------------------------------------------------

specify_block :
        'specify' (specify_item)* 'endspecify'
        ;

specify_item :
        spec_param_declaration |
        (path_declaration) => path_declaration |
        system_timing_check
        | sdpd
        ;

spec_param_declaration :
        'specparam' list_of_specparam_assignments SEMI
        ;

list_of_specparam_assignments :
        specparam_assignment ( COMMA specparam_assignment )*
        ;

specparam_assignment :
        identifier ASSIGN expression
        ;

path_declaration :
        (simple_path_declaration) =>
	   simple_path_declaration SEMI |
        (level_sensitive_path_declaration) =>
	   level_sensitive_path_declaration SEMI |
        edge_sensitive_path_declaration SEMI
        ;

simple_path_declaration :
        (parallel_path_description) =>
	   parallel_path_description ASSIGN path_delay_value |
        full_path_descriptor ASSIGN path_delay_value
        ;

parallel_path_description :
        LPAREN specify_terminal_descriptor PPATH specify_terminal_descriptor RPAREN
        ;

full_path_descriptor :
        LPAREN list_of_path_terminals  FPATH list_of_path_terminals RPAREN
        ;

list_of_path_terminals :
        specify_terminal_descriptor ( COMMA specify_terminal_descriptor )*
        ;

specify_terminal_descriptor :
	(identifier LBRACK expression COLON) =>
           identifier LBRACK expression COLON expression RBRACK |
        (identifier LBRACK) =>
           identifier LBRACK expression RBRACK |
        identifier
        ;

path_delay_value :
        (path_delay_expression) => path_delay_expression |
        LPAREN list_of_path_delay_expressions RPAREN
        ;

list_of_path_delay_expressions :
        path_delay_expression COMMA path_delay_expression
	  ( COMMA path_delay_expression
	    ( COMMA path_delay_expression COMMA
              path_delay_expression COMMA path_delay_expression )? )?
        ; 

path_delay_expression :
        mintypmax_expression
        ;

system_timing_check :
        '$setup' LPAREN timing_check_event COMMA timing_check_event COMMA
            timing_check_limit ( COMMA notify_register )? RPAREN SEMI |
        '$hold' LPAREN timing_check_event COMMA timing_check_event COMMA
            timing_check_limit ( COMMA notify_register )? RPAREN SEMI |
        '$period' LPAREN controlled_timing_check_event COMMA
            timing_check_limit ( COMMA notify_register )? RPAREN SEMI |
        '$width' LPAREN controlled_timing_check_event COMMA
            timing_check_limit ( COMMA expression COMMA notify_register )?
	    RPAREN SEMI |
        '$skew' LPAREN timing_check_event COMMA timing_check_event COMMA 
            timing_check_limit ( COMMA notify_register )? RPAREN SEMI |
        '$recovery' LPAREN controlled_timing_check_event COMMA
            timing_check_event COMMA timing_check_limit
            ( COMMA notify_register )? RPAREN SEMI |
        '$setuphold' LPAREN timing_check_event COMMA timing_check_event COMMA
            timing_check_limit COMMA timing_check_limit
            ( COMMA notify_register )? RPAREN SEMI
        ;

timing_check_event :
        (timing_check_event_control)? specify_terminal_descriptor
        ( '&&&' timing_check_condition )?
        ;

controlled_timing_check_event :
        timing_check_event_control specify_terminal_descriptor
        ( '&&&' timing_check_condition )?
        ;

timing_check_event_control :
        'posedge' |
        'negedge' |
        edge_control_specifier
        ;

edge_control_specifier :
        'edge' LBRACK edge_descriptor ( COMMA edge_descriptor )* RBRACK
        ;

   // Use semantic predicates to determine whether a matched
   // NUMBER or IDENTIFIER is a valid special value in the given context.
   // This kludge avoids having the special values in the Literals table,
   // thus avoiding a lexical conflict.
edge_descriptor :
	'0x' | '1x'
      |
	n=NUMBER
	{ $n.text=="01" || $n.text=="10"}?
      |
	i=IDENTIFIER
	{ $i.text=="x1" || $i.text=="x0"}?
	;

timing_check_condition :
        scalar_timing_check_condition
        ; 
scalar_timing_check_condition :
        expression
        ;

timing_check_limit :
        expression
        ;

notify_register :
        name_of_register
        ;

level_sensitive_path_declaration :
	(parallel_level_sensitive_path_description) =>
	parallel_level_sensitive_path_description
	     ASSIGN path_delay_value SEMI
      |
	full_level_sensitive_path_description
	     ASSIGN path_delay_value SEMI
        ;

parallel_level_sensitive_path_description :
        'if' LPAREN expression RPAREN
	   LPAREN specify_terminal_descriptor (polarity_operator)?
	          PPATH specify_terminal_descriptor RPAREN
	;

full_level_sensitive_path_description :
        'if' LPAREN expression RPAREN
	   LPAREN list_of_path_terminals (polarity_operator)?
	          FPATH list_of_path_terminals RPAREN
	;

polarity_operator :
	PLUS |
	MINUS
	;

edge_sensitive_path_declaration :
	( 'if' LPAREN expression RPAREN )?
        LPAREN (edge_identifier)? specify_terminal_descriptor
	   ( PPATH | FPATH )
	   LPAREN ( (list_of_path_terminals) => list_of_path_terminals |
	            specify_terminal_descriptor )
	      (polarity_operator)? COLON data_source_expression
	   RPAREN
	RPAREN
	ASSIGN path_delay_value SEMI
        ;

data_source_expression :
        expression
        ;

edge_identifier :
        'posedge' |
        'negedge'
        ;

sdpd :
	'if' LPAREN expression RPAREN
	simple_path_declaration
	SEMI
	;

//----------------------------------------------------------------------------
// Expressions
//----------------------------------------------------------------------------

lvalue :
	(identifier range) =>
        identifier range |
        identifier |
        concatenation
        ;

concatenation :
	(LCURLY expression LCURLY) =>
        LCURLY expression
	       LCURLY expression ( COMMA expression )* RCURLY RCURLY |
        LCURLY expression ( COMMA expression )* RCURLY
        ;

mintypmax_expression :
        expression ( COLON expression COLON expression )?
        ;

exp11 :
        STRING |
	NUMBER |
	(function_call) => function_call |
	lvalue |
	DEFINE
        ;

exp10 :
        exp11 | LPAREN expression RPAREN
        ;

exp9 :
        exp10 | unary_operator exp9
        ;

exp8 :
        exp9 ( binary_operator exp9 )*
        ;

exp7 :
        exp8 ( QUESTION exp7 COLON exp7 )?
        ;

exp0 :
        exp7
        ;

expression :
        exp0
        ;

function_call :
        name_of_function LPAREN expression_list RPAREN |
        SYSTEM_TASK_NAME ( LPAREN expression_list RPAREN )?
        ;

expression_list :
        expression ( COMMA expression )*
        ;

unary_operator :
        PLUS   |
        MINUS  |
        LNOT   |
        BNOT   |
        BAND   |
        RNAND  |
        BOR    |
        RNOR   |
        BXOR   |
        BXNOR 
        ;

binary_operator :
        PLUS        |
        MINUS       |
        STAR        |
        DIV         |
        MOD         |
        EQUAL       |
        NOT_EQ      |
        EQ_CASE     |
        NOT_EQ_CASE |
        LAND        |
        LOR         |
        LT_         |
        LE          |
        GT          |
        GE          |
        BAND        |
        BOR         |
        BXOR        |
        BXNOR       |
        SR          |
        SL 
        ;


KW_ASSIGN				:	'assign'				;
KW_TRIREG				:	'trireg'				;

//----------------------------------------------------------------------------
// Identifiers
//----------------------------------------------------------------------------

name_of_module :            local_identifier ;
name_of_port :              local_identifier ;
name_of_variable :          local_identifier ;
name_of_UDP :               local_identifier ;
name_of_UDP_instance :      local_identifier ;
name_of_event :             local_identifier ;
name_of_task :              local_identifier ;
real_identifier :           identifier ;
name_of_memory :            local_identifier ;
net_identifier :            identifier ;
name_of_function :          local_identifier ;
specparam_identifier :      identifier ;
udp_name_of_port :          identifier ;
name_of_register :          local_identifier ;
name_of_gate_instance :     local_identifier ;
name_of_instance :          local_identifier ;
name_of_block :             local_identifier ;
output_terminal_name :      local_identifier ;


//----------------------------------------------------------------------------
// General
//----------------------------------------------------------------------------

identifier :
        identifier_path
        ;

identifier_path :
        local_identifier ( DOT local_identifier )*
        ;

local_identifier :
        IDENTIFIER |
        ESCAPED_IDENTIFIER
        ;

delay_control :
        POUND NUMBER |
        POUND identifier |
        POUND LPAREN mintypmax_expression RPAREN
        ;

event_control :
        AT identifier |
        AT LPAREN event_expression RPAREN
        ;

event_expression :
        sub_event_expression ( 'or' sub_event_expression )*
        ;

sub_event_expression :
        expression |
        'posedge' expression |
        'negedge' expression
        ;

//----------------------------------------------------------------------------
// Compiler directives
//----------------------------------------------------------------------------

directive:
	define_directive |
	include_directive
	;

define_directive :
	'`define' IDENTIFIER expression
	;

include_directive :
	'`include' ( identifier | STRING )
	;

//----------------------------------------------------------------------------
//----------------------------------------------------------------------------
// The Verilog scanner
//----------------------------------------------------------------------------


  // Operators
AT	    : '@'   ;
COLON	    : ':'   ;
COMMA	    : ','   ;
DOT	    : '.'   ;
ASSIGN	    : '='   ;
MINUS	    : '-'   ;
LBRACK	    : '['   ;
RBRACK	    : ']'   ;
LCURLY	    : '{'   ;
RCURLY	    : '}'   ;
LPAREN	    : '('   ;
RPAREN	    : ')'   ;
POUND	    : '#'   ;
QUESTION    : '?'   ;
SEMI	    : ';'   ;
PLUS        : '+'   ;
LNOT        : '!'   ;
BNOT        : '~'   ;
BAND        : '&'   ;
RNAND       : '~&'  ;
BOR         : '|'   ;
RNOR        : '~|'  ;
BXOR        : '^'   ;
BXNOR       : '~^' | '^~' ;
STAR        : '*'   ;
DIV         : '/'   ;
MOD         : '%'   ;
EQUAL       : '=='  ;
NOT_EQ      : '!='  ;
NOT_EQ_CASE : '!==' ;
EQ_CASE     : '===' ;
LAND        : '&&'  ;
LOR         : '||'  ;
LT_         : '<'   ;
LE          : '<='  ;
GT          : '>'   ;
GE          : '>='  ;
SR          : '>>'  ;
SL          : '<<'  ;
TRIGGER     : '->'  ;
PPATH       : '=>'  ;
FPATH       : '*>'  ;

    // an identifier.  Note that testLiterals is set to true!  This means
    // that after we match the rule, we look in the Literals table to see
    // if it's a literal or really an identifer.
IDENTIFIER
      //options {testLiterals=true;}
    :
        ('a'..'z'|'A'..'Z'|'_') ('a'..'z'|'A'..'Z'|'_'|'$'|'0'..'9')*
        ;


/*ESCAPED_IDENTIFIER :
        '\\'! (~ '\u0020')+ ('\u0020'|'\t'|'\n')!
        ;*/

ESCAPED_IDENTIFIER :
        '\\' (~ '\u0020')+ ('\u0020'|'\t'|'\n')
        ;

SYSTEM_TASK_NAME :
        '$' IDENTIFIER
        ;

    // string literals
STRING :
        '"' (~('"' | '\n'))* '"'
        ;

    // 'compiler' define/macro.
DEFINE
      //options {testLiterals=true;}
    :
	'`' IDENTIFIER
	;

   // a dummy rule to force vocabulary to be all characters (except special
   //   ones that ANTLR uses internally (0 to 2)
fragment
VOCAB :
        '\u0003'..'\u007F'
        ;

   // a numeric literal
NUMBER :
	( (SIZE)? BASE SIZED_DIGIT ) => SIZED_NUMBER |
	UNSIZED_NUMBER
	;

fragment
SIZED_NUMBER :
	(SIZE)? BASE SIZED_DIGIT (SIZED_DIGIT | '_')*
	;

fragment
SIZE :
	(DIGIT)+
	;

fragment
BASE :
	'\'' ( 'd' | 'D' | 'h' | 'H' | 'o' | 'O' | 'b' | 'B' )
	;

fragment
SIZED_DIGIT :
	DIGIT | HEXDIGIT | 'x' | 'X' | 'z' | 'Z' | '?'
	;

fragment
UNSIZED_NUMBER :
	DIGIT (DIGIT | '_')* ( '.' (DIGIT | '_')* )? (EXPONENT)?
        ;

fragment
DIGIT :
        ('0'..'9')
        ;

fragment
HEXDIGIT :
        ('A'..'F'|'a'..'f')
        ;

fragment
EXPONENT :
        ('e'|'E') ('+'|'-')? ('0'..'9')+
        ;

fragment SPACE_OR_TAB
	:	(
			' '
		|	'\t'
		)+
	;

WS
	:	SPACE_OR_TAB+
		{$channel=HIDDEN;}
	;
    
ML_COMMENT
    :   '/*' ( options {greedy=false;} : . )* '*/' {$channel=HIDDEN;}
    ;

SL_COMMENT
    : '//'  ( options {greedy=false;} : . )*  '\r'? '\n' {$channel=HIDDEN;}
    ;