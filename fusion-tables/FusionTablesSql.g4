/*
 * This is free and unencumbered software released into the public domain.
 * 
 * Anyone is free to copy, modify, publish, use, compile, sell, or
 * distribute this software, either in source code form or as a compiled
 * binary, for any purpose, commercial or non-commercial, and by any
 * means.
 * 
 * In jurisdictions that recognize copyright laws, the author or authors
 * of this software dedicate any and all copyright interest in the
 * software to the public domain. We make this dedication for the benefit
 * of the public at large and to the detriment of our heirs and
 * successors. We intend this dedication to be an overt act of
 * relinquishment in perpetuity of all present and future rights to this
 * software under copyright law.
 * 
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
 * EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
 * MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
 * IN NO EVENT SHALL THE AUTHORS BE LIABLE FOR ANY CLAIM, DAMAGES OR
 * OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE,
 * ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
 * OTHER DEALINGS IN THE SOFTWARE.
 * 
 * @author Stefan Smertnig   (stef)
 * @author Curiosa Globunznik (curiosa)
 * @version $Id: FusionTablesSql.g4 1 2016-01-01 00:00:00MET curiosa $
 */
 
 
grammar FusionTablesSql;

// credits to bart kier. Took his SQLite grammar as a starting point ant went a long way from there 

K_ALTER : A L T E R;
K_AND : A N D;
K_OR : O R;
K_AS : A S;
K_ASC : A S C;
K_AVERAGE: A V E R A G E;
K_BY : B Y;
K_BETWEEN : B E T W E E N;
K_CASE : C A S E;
K_CIRCLE: C I R C L E;
K_COLUMN : C O L U M N;
K_CONTAINS : C O N T A I N S;
K_COUNT : C O U N T;
K_CREATE : C R E A T E;
K_DELETE : D E L E T E;
K_DESC : D E S C;
K_DESCRIBE : D E S C R I B E;
K_DOES : D O E S;
K_CONTAIN : C O N T A I N;
K_DROP : D R O P;
K_ENDS : E N D S;
K_FROM : F R O M;
K_GROUP : G R O U P;
K_HAVING : H A V I N G;
K_IGNORING : I G N O R I N G;
K_IN : I N;
K_INSERT : I N S E R T;
K_INTO : I N T O;
K_JOIN : J O I N;
K_LATLNG : L A T L N G;
K_LEFT : L E F T;
K_LIKE : L I K E;
K_LIMIT : L I M I T;
K_MATCHES : M A T C H E S;
K_MAXIMUM : M A X I M U M;
K_MINIMUM : M I N I M U M;
K_NOT : N O T;
K_EQUAL : E Q U A L;
K_OF : O F;
K_OFFSET : O F F S E T;
K_ON : O N;
K_ORDER : O R D E R;
K_OUTER : O U T E R;
K_RECTANGLE : R E C T A N G L E;
K_RENAME : R E N A M E;
K_ST_DISTANCE : S T '_' D I S T A N C E;
K_SELECT : S E L E C T;
K_ST_INTERSECTS : S T '_' I N T E R S E C T S;
K_SUM : S U M;
K_SET : S E T;
K_SHOW : S H O W;
K_STARTS : S T A R T S;
K_TABLE : T A B L E;
K_TABLES : T A B L E S;
K_TO : T O;
K_UPDATE : U P D A T E;
K_VALUES : V A L U E S;
K_VIEW : V I E W;
K_WHERE : W H E R E;
K_WITH : W I T H;

fusionTablesSql
 : sql_stmt * ; 

sql_stmt
 : ( alter_table_stmt
    | select_stmt
    | create_view_stmt
    | create_table_as_select_stmt
    | delete_stmt
    | drop_table_stmt
    | insert_stmt
    | update_stmt
    | describe_stmt
    | show_tables_stmt
   ) ';' 
 ;

table_name_in_ddl
 : table_name
 ;
 
table_name_in_dml
 : table_name
 ;

create_table_as_select_stmt
 : K_CREATE K_TABLE identifier K_AS K_SELECT '*' K_FROM table_name_in_ddl
 ; 

describe_stmt
 : K_DESCRIBE table_name_in_ddl
 ;

show_tables_stmt
 : K_SHOW K_TABLES
 ;

alter_table_stmt
 : K_ALTER K_TABLE table_name_in_ddl
   ( K_RENAME K_TO identifier )
 ;

create_view_stmt
 : K_CREATE K_VIEW view_name K_AS 
     '(' 
    	K_SELECT result_column ( ',' result_column )*
    	K_FROM ((table_name_with_alias ( K_WHERE expr )?) (join_clause) *)
     ')'
 ;

drop_table_stmt
 : K_DROP K_TABLE table_name_in_ddl
 ;

insert_stmt
  : K_INSERT  K_INTO
   table_name_in_dml ( '(' column_name_in_dml ( ',' column_name_in_dml )* ')' )
   ( K_VALUES '(' literal ( ',' literal)* ')' )
 ;

update_stmt
 : K_UPDATE table_name_in_dml
   K_SET column_assignment ( ',' column_assignment )* 
   K_WHERE eq_comparison
 ;

column_assignment : column_name_in_dml '=' literal ;

delete_stmt
 : K_DELETE K_FROM table_name_in_dml 
   ( K_WHERE column_name_in_dml '=' literal)?
 ;

eq_comparison : identifier EQ string_literal ;

table_name_with_alias
 : table_name ( K_AS table_alias )?
 ;

select_stmt
 : K_SELECT result_column ( ',' result_column )*
   K_FROM table_name_with_alias (join_clause )*  
   ( K_WHERE expr )?
   ( K_GROUP K_BY qualified_column_name ( ',' qualified_column_name )* )?
   ( K_ORDER K_BY ordering_term ( ',' ordering_term )* )?
   ( 
   	 ( K_OFFSET numeric_literal ( K_LIMIT numeric_literal )?) 
   )?  
 ;

ordering_term
 : (qualified_column_name | K_ST_DISTANCE '(' qualified_column_name ',' coordinate ')') ( K_ASC | K_DESC )?
;

join_clause
 : ( K_LEFT K_OUTER  K_JOIN table_name_with_alias K_ON qualified_column_name '=' qualified_column_name)
 ;
  
result_column
 : '*'
 | table_name '.' '*'
 | qualified_column_name
 | aggregate_exp
 ;
 
qualified_column_name : ( table_name '.' )? column_name
;
 
aggregate_exp : ( K_SUM | K_COUNT | K_AVERAGE | K_MAXIMUM | K_MINIMUM ) LPAR qualified_column_name RPAR;

expr
 : column_name_beginning_expr ( operator ) literal (and_or_or expr)? 
 | column_name_beginning_expr ( K_LIKE | K_MATCHES | K_STARTS K_WITH | K_ENDS K_WITH | K_CONTAINS | K_CONTAINS K_IGNORING K_CASE | K_DOES K_NOT K_CONTAIN | K_NOT K_EQUAL K_TO) string_literal (and_or_or expr)?
 | column_name_beginning_expr K_IN '(' string_literal ( ',' string_literal ) * ')' (and_or_or expr)?
 | column_name_beginning_expr K_BETWEEN literal K_AND literal (and_or_or expr)?
 | K_ST_INTERSECTS LPAR qualified_column_name ',' geometry RPAR (and_or_or expr)?
 ;

column_name_beginning_expr
 :  qualified_column_name  
 ;

column_name_in_dml
 :  column_name  
 ;

and_or_or : (K_AND | K_OR) ; 

geometry : circle | rectangle ;
	
circle : K_CIRCLE  '(' coordinate ',' numeric_literal ')' ;

rectangle :  K_RECTANGLE '(' coordinate ',' coordinate ')' ;

coordinate : K_LATLNG '(' numeric_literal ',' numeric_literal ')' ;

keyword
 :   
 | K_ALTER
 | K_AND
 | K_OR
 | K_AS
 | K_ASC
 | K_AVERAGE
 | K_BY
 | K_BETWEEN
 | K_CASE 
 | K_CIRCLE
 | K_COLUMN
 | K_CONTAIN
 | K_CONTAINS
 | K_COUNT
 | K_CREATE
 | K_DELETE
 | K_DESC
 | K_DOES
 | K_DROP
 | K_ENDS
 | K_EQUAL
 | K_FROM
 | K_GROUP
 | K_HAVING
 | K_IGNORING
 | K_IN
 | K_INSERT
 | K_INTO
 | K_JOIN
 | K_LATLNG
 | K_LEFT
 | K_LIKE
 | K_LIMIT
 | K_MATCHES
 | K_MAXIMUM
 | K_MINIMUM
 | K_NOT
 | K_OF
 | K_OFFSET
 | K_ON
 | K_ORDER
 | K_OUTER
 | K_RECTANGLE
 | K_RENAME
 | K_SELECT
 | K_SET
 | K_STARTS
 | K_ST_DISTANCE
 | K_ST_INTERSECTS
 | K_SUM
 | K_TABLE
 | K_TO
 | K_UPDATE
 | K_VALUES
 | K_VIEW
 | K_WHERE
 | K_WITH
 ;

operator 
 : LT
 | LT_EQ
 | GT
 | GT_EQ
 | EQ
 ;

literal
 : numeric_literal
 | string_literal
 ;
 
error_message
 : string_literal
 ;

identifier: string_literal;

column_alias
 : identifier
 ;      

table_name 
 : identifier
 ;

column_name 
 : identifier
 ;

new_table_name 
 : table_name
 ;

view_name 
 : identifier
 ;

table_alias 
 : identifier
 ;

numeric_literal : NUMERIC_LITERAL ;

string_literal : STRING_LITERAL ;

LT_EQ : '<=';
GT_EQ : '>=';
GT : '>';
EQ : '=';
LT : '<';
LPAR : '(';
RPAR : ')';

NUMERIC_LITERAL
 : DIGIT+ ( '.' DIGIT* )? 
 | ( '+' | '-' ) NUMERIC_LITERAL
 ;

STRING_LITERAL
 : STRING
 | QUOTED_STRING
 ;

STRING : ([a-zA-Z_0-9] | '-')+ // TODO unicode support
	   ;

QUOTED_STRING
 : '\'' ( ~'\'' | '\'\'' )* '\''
 ;

SINGLELINE_COMMENT
 : '--' ~[\r\n]* -> channel(HIDDEN)
 ;

MULTILINE_COMMENT
 : '/*' .*? ( '*/' | EOF ) -> channel(HIDDEN)
 ;

WHITESPACE
 : [ \u000B\t\r\n] -> channel(HIDDEN)
 ;



fragment DIGIT : [0-9];
fragment A : [aA];
fragment B : [bB];
fragment C : [cC];
fragment D : [dD];
fragment E : [eE];
fragment F : [fF];
fragment G : [gG];
fragment H : [hH];
fragment I : [iI];
fragment J : [jJ];
fragment K : [kK];
fragment L : [lL];
fragment M : [mM];
fragment N : [nN];
fragment O : [oO];
fragment P : [pP];
fragment Q : [qQ];
fragment R : [rR];
fragment S : [sS];
fragment T : [tT];
fragment U : [uU];
fragment V : [vV];
fragment W : [wW];
fragment X : [xX];
fragment Y : [yY];
fragment Z : [zZ];

