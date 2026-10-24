parser grammar TeradataSQLExpressionsParser;

import TeradataSQLLiteralsParser
     , TeradataSQLIdentifiersParser
     , TeradataSQLDataTypesParser
     , TeradataSQLRequestModifiersParser
     ;

options {
    tokenVocab=TeradataSQLLexer;
}

/*******************
    Query expression
*/
/* I've decided to create one generic select rule to minimize copy-and-paste. In fact, there are some differences
when using select rule depending on context:
- in CREATE/REPLACE view it is possible to specify WITH CHECK OPTION clause between other clauses of SELECT expression;
- INTO clause is only allowed when using SELECT inside procedures;
In my opinion those cases should be handled when analyzing parsed tree, and it seems that Teradata query processor does exactly that.
*/
query_expr
    : query_term
    /* The following restrictions should be handled after parsing:
        - A query expression must contain only one ORDER BY specification, at the end.
        - SELECT statements connected by set operators could include any of the common clause options for SELECT
        except the WITH clause.
        - INTERSECT, MINUS/EXCEPT, UNION cannot be used within the following:
            ◦ SELECT AND CONSUME statements.
            ◦ WITH RECURSIVE clause
            ◦ CREATE RECURSIVE VIEW statements
        - UNION ALL could be used with CREATE RECURSIVE VIEW
    */
    | query_expr (UNION|MINUS|EXCEPT|INTERSECT) ALL? query_expr
    | '(' query_expr ')'
    ;

query_term
    : (SELECT|SEL)
      with_deleted_rows?
      as_json?
      select_list
    /*
    The documented syntax specifies the ANSI SQL:2011 ordering of these clauses: FROM, WHERE, GROUP BY, HAVING,
    and ORDER BY. Teradata Database does not enforce this ordering
    */
      ( from_clause with_clause*
      | into_clause
      | where_clause
      | group_by_clause // Do not combine WITH and GROUP BY clauses in a SELECT statement (though it is possible) .
      | (having_clause|qualify_clause)
      | sample_clause // See documentation for SAMPLE clause
      | with_clause* order_by_clause with_clause* //You can specify an ORDER BY clause *before* or after any WITH clause.
      | with_check_option // this option is used only in CREATE/REPLACE view
      )*
      expand_on_clause?
    ;

with_deleted_rows : WITH DELETED ROWS ;

as_json : AS JSON ;

select_list
    : ( ( DISTINCT
        | UNIQUE
        | ALL
        // You cannot include NORMALIZE in a recursive statement of a recursive query.
        | normalize
        )
      | top_n
      )?
      selected_columns
    | all_operator
    ;

top_n : TOP (integer_literal|float_literal|macro_parameter_reference) PERCENT? (WITH TIES)? ;

normalize
    : NORMALIZE
      ( ON MEETS OR OVERLAPS
      | ON OVERLAPS (OR MEETS)?
      )?
    ;

all_operator : ('*'|table_name '.' '*'|column_name) '.' ALL ;

selected_columns
    : all_rows='*'
    | selected_column (',' selected_column)*
    ;

selected_column
    : scalar_expr (AS? alias_name)?
    | table_name '.' '*'
    ;

into_clause : INTO (variable_name|variable_reference) (',' (variable_name|variable_reference))* ;

from_clause : FROM from_spec ;

from_spec
    : join_source_spec ( join_joined_spec )*
    | from_pivot_spec ( join_joined_spec )*
    | from_unpivot_spec ( join_joined_spec )*
    | '(' from_spec ')' ( join_joined_spec )*
    ;

join_source_spec
    : table_reference
    | subquery_reference
    | foreign_table_reference
    | table_function_reference
    | foreign_function_reference
    | table_operator_reference
    ;

join_joined_spec
    : ',' table_reference
    | ',' subquery_reference
    | ',' table_function_reference
    | ',' table_operator_reference
    | join_clause
    ;

from_pivot_spec : (table_reference|subquery_reference) pivot ;

from_unpivot_spec : (table_reference|subquery_reference) unpivot ;

table_reference
    : table_name (server_name_reference foreign_returns_clause? )?
      (AS? table_alias=alias_name)?
    ;

join_clause
    : (INNER|(LEFT|RIGHT|FULL) OUTER?)? JOIN join_source_spec join_on_clause* // YES, this is valid syntax (FROM a LEFT JOIN b LEFT JOIN c ON condition ON condition)
    | (INNER|(LEFT|RIGHT|FULL) OUTER?)? JOIN '(' from_spec ')' join_on_clause* // for cases like "JOIN (tbl JOIN other_tbl)"
    | CROSS JOIN join_source_spec join_on_clause*                              // and for CROSS JOIN too
    | CROSS JOIN '(' from_spec ')' join_on_clause*
    ;

join_on_clause : ON logical_expr ;

foreign_table_reference
    : FOREIGN TABLE '(' (query_term|foreign_sql=.+?) ')' server_name_reference
      (EXPORT '(' exported_data (',' exported_data)* ')' )?
      AS? table_alias=alias_name
    ;

foreign_function_reference
    : function_name server_name_reference '(' foreign_on_clause+ ')' AS? alias_name
    ;

foreign_on_clause
    : ON '(' scalar_expr (',' scalar_expr)* ')' //TODO: inferred from doc example, check if this syntax is correct
      (hash_or_partition_by /*| dimension_clause TODO: found it in HELP 'sql select', but without definition */)*
      (LOCAL? order_by_clause)?
      foreign_using_clause // TODO: check if this clause is mandatory
    ;

exported_data
    : exported_subquery=subquery_reference
    | exported_table=table_name AS? table_alias=alias_name
    ;

foreign_using_clause : USING foreign_parameter+ ;

foreign_parameter : name=unqualified_name '(' value=scalar_expr ')' ;

foreign_returns_clause : RETURNS '(' column_spec (',' column_spec)* ')' ;

server_name_reference : '@' alias_name ;

table_function_reference : TABLE '(' (udt_table_function|unnest_table_function) ')' AS? alias_name column_list? ;

udt_table_function
    : function_name ('(' ')'|scalar_expr_list_comma_separated)
        table_function_returns_clause?
        (table_function_local_order_by_clause|table_function_hash_by_clause)*
    ;

unnest_table_function
    : td_sysfnlib? UNNEST '(' (key_expr=scalar_expr ',')? array_expr=scalar_expr ')' (WITH ORDINALITY)?
    ;

table_function_returns_clause : RETURNS (returned_table_name=table_name| '(' column_spec (',' column_spec)* ')' )? ;

table_function_local_order_by_clause : LOCAL ORDER BY order_by_spec_full (',' order_by_spec_full)* ;

table_function_hash_by_clause : HASH BY column_name (',' column_name)* ;

table_operator_reference
    :  ( xmltable_operator
       | calcmatrix_table_operator
       | read_nos_table_operator
       | script_table_operator
       | td_unpivot_table_operator
       | write_nos_table_operator
       | json_keys_table_operator
       | json_table_table_operator
       | json_shred_table_operator
       | generic_table_operator
       ) AS? alias_name column_list?
    ;

xmltable_operator : XMLTABLE '(' xml_namespace_declaration? xml_tab_row_pattern=scalar_expr xml_query_argument? xml_columns_spec? ')' ;

calcmatrix_table_operator
    : td_sysfnlib? CALCMATRIX '('
      ON (table_name|subquery)
      (HASH BY scalar_expr (',' scalar_expr)* )?
      (LOCAL ORDER BY scalar_expr (',' scalar_expr)* )?
      USING PHRASE '(' using_phrase=char_string_literal ')' // 'LOCAL', 'COMBINE'
      (CALCTYPE '(' calc_type=char_string_literal')' )? // 'SSCP', 'ESSCP', 'CSSCP', 'COV', 'COR'
      (OUTPUT '(' output=char_string_literal')' )? // 'COLUMNS', 'VARBYTE'
      (NULL_HANDLING '(' null_handling=char_string_literal')' )? // 'ZERO', 'IGNORE'
    ;

read_nos_table_operator : READ_NOS '(' ON (table_name|subquery) USING '(' location read_nos_option* ')' ')'
    ;

script_table_operator
    : SCRIPT '(' table_operator_on_clause
      (RETURNS '(' ('*'|return_spec+=char_string_literal (',' return_spec+=char_string_literal)* ) ')' )?
      (DELIMITER '(' delimiter_char=char_string_literal ')' )?
      (CHARSET '(' charset=char_string_literal ')' )? // 'UTF-16'|'LATIN'
      (QUOTECHAR '(' quote_char=char_string_literal ')' )?
      (AUTH '(' authorization_name=object_name ')' )? ')'
    ;

td_unpivot_table_operator
    : td_sysfnlib? TD_UNPIVOT '(' ON (table_name|subquery)
      USING VALUE_COLUMNS '(' value_columns_value+=char_string_literal (',' value_columns_value+=char_string_literal)* ')'
      UNPIVOT_COLUMN '(' unpivot_column_value=char_string_literal ')'
      COLUMN_LIST '(' column_list_value+=char_string_literal (',' column_list_value+=char_string_literal)* ')'
      ( COLUMN_ALIAS_LIST '(' column_alias_list_value+=char_string_literal (',' column_alias_list_value+=char_string_literal)* ')'
        INCLUDE_NULLS '(' include_nulls=char_string_literal ')' // 'Yes'|'No'
      )?
    ;

write_nos_table_operator : WRITE_NOS '(' table_operator_on_clause USING '(' location write_nos_option* ')' ')' ;

json_table_table_operator
    : td_sysfnlib? JSON_TABLE '(' ON subquery
        USING ROWEXPR '(' row_expr_literal=char_string_literal ')'
        COLEXPR '(' col_expr_literal=char_string_literal ')' ')'
    ;

json_keys_table_operator
    : td_sysfnlib? JSON_KEYS '(' ON subquery (USING json_keys_using_name_value_pair+ )? ')'
    ;

json_shred_table_operator
    : td_sysfnlib? TD_JSONSHRED '(' ON subquery
        USING ROWEXPR '(' row_expr_literal=char_string_literal ')'
        COLEXPR '(' col_expr_literal+=char_string_literal (',' col_expr_literal+=char_string_literal)* ')'
        RETURNTYPES '(' return_type+=char_string_literal (',' return_type+=char_string_literal)* ')'
        (NOCASE '(' nocase_value=integer_literal')' )?
        (TRUNCATE '(' truncate_value=integer_literal')' )?
    ')'
    ;

generic_table_operator
    : table_name '('
      table_operator_on_clause*
      table_operator_execute_clause?
      table_operator_out_table_clause*
      table_operator_using_clause? ')'
   ;

table_operator_on_clause
    : ON (table_name|subquery) (AS alias_name)?
      (hash_or_partition_by|DIMENSION)?
      (LOCAL? ORDER BY order_by_spec_full (',' order_by_spec_full)* )?
    ;

table_operator_execute_clause
    : EXECUTE MAP '=' map_name=char_string_literal (COLOCATE USING '=' colocation_name=char_string_literal)?
    ;

table_operator_out_table_clause : OUT TABLE name=unqualified_name '(' table_name ')' ;

table_operator_using_clause : USING table_operator_using_spec+ ;

table_operator_using_spec : name=unqualified_name (scalar_expr_list_comma_separated|subquery) ;

json_keys_using_name_value_pair : (DEPTH|QUOTES) '(' value=scalar_expr ')' ;

hash_or_partition_by
    : (HASH|PARTITION) BY scalar_expr (',' scalar_expr)*
    | PARTITION BY ANY
    ;

subquery_reference : subquery AS? alias_name column_list? ;

location : LOCATION '(' external_file_path=CHAR_STRING ')' ;

read_nos_option
    : AUTHORIZATION '(' (authorization_name=object_name|authorization_json=char_string_literal) ')' // '{"Access_ID":"identification", "Access_Key":"secret_key"}'
    | BUFFERSIZE '(' buffer_size=char_string_literal ')'
    | RETURNTYPE '(' return_type=char_string_literal ')' // 'NOSREAD_RECORD' | 'NOSREAD_KEYS' | 'NOSREAD_PARQUET_SCHEMA' 'NOSREAD_SCHEMA'
    | SAMPLE_PERC '(' row_sampling_value=char_string_literal ')'
    | STOREDAS '(' stored_as=char_string_literal ')' // 'PARQUET'|'TEXTFILE'
    | FULLSCAN '(' fullscan=char_string_literal ')' // 'TRUE'|'FALSE'
    | MANIFEST '(' manifest=char_string_literal ')' // 'TRUE'|'FALSE'
    | ROWFORMAT '(' row_format=char_string_literal ')'
    | HEADER '(' header=char_string_literal ')' // 'TRUE'|'FALSE'
    ;

write_nos_option
    : AUTHORIZATION '(' (authorization_name=object_name|authorization_json=char_string_literal) ')' // '{"Access_ID":"identification", "Access_Key":"secret_key"}'
    | STOREDAS '(' stored_as=char_string_literal ')' // 'PARQUET'
    | NAMING '(' naming=char_string_literal ')' // 'DISCRETE'|'RANGE'
    | MANIFESTFILE '(' manifest_name=char_string_literal ')'
    | MANIFESTONLY '(' manifest_only=char_string_literal ')' // 'TRUE'|'FALSE'
    | OVERWRITE '(' overwrite=char_string_literal ')' // 'TRUE'|'FALSE'
    | INCLUDE_ORDERING '(' include_ordering=char_string_literal ')' // 'TRUE'|'FALSE'
    | INCLUDE_HASHBY '(' include_hashby=char_string_literal ')' // 'TRUE'|'FALSE'
    | MAXOBJECTSIZE '(' max_object_size=char_string_literal ')'
    | COMPRESSION '(' compression=char_string_literal ')' // 'GZIP'|'SNAPPY'
    ;

/* WITH clause */
with_clause
    : WITH scalar_expr with_clause_title_phrase?  (',' scalar_expr with_clause_title_phrase?)*
      with_clause_by_phrase?
    ;

with_clause_by_phrase : BY order_by_spec_asc_desc_only (',' order_by_spec_asc_desc_only)* ;

with_clause_title_phrase : '(' TITLE title=char_string_literal ')';

/* WHERE clause */
where_clause : WHERE logical_expr ;

/* GROUP BY clause */
group_by_clause
    : GROUP BY group_by_spec
    | GROUP BY group_by_spec (',' group_by_spec )+
    ;

group_by_spec
    : ordinary_grouping_set
    | rollup_option
    | cube_option
    | grouping_sets_option
    ;

ordinary_grouping_set
    : (scalar_expr|ordinary_grouping_set_parenthesized) (',' (scalar_expr|ordinary_grouping_set_parenthesized) )*
    ;

ordinary_grouping_set_parenthesized : '(' scalar_expr (',' scalar_expr )+ ')' ;


empty_grouping_set : '(' ')' ;

rollup_option : ROLLUP '(' ordinary_grouping_set ')' ;

cube_option : CUBE '(' ordinary_grouping_set ')' ;

grouping_sets_option : GROUPING SETS '(' grouping_sets_spec (',' grouping_sets_spec)* ')' ;

grouping_sets_spec
    : ordinary_grouping_set
    | empty_grouping_set
    | rollup_option
    | cube_option
    | grouping_sets_option
    ;

/* HAVING clause */
having_clause : HAVING logical_expr ;

/* QUALIFY clause */
qualify_clause : QUALIFY logical_expr ;

/* SAMPLE clause */
sample_clause
    : SAMPLE (WITH REPLACEMENT)? (RANDOMIZED ALLOCATION)?
      ( sample_fraction_description
      | sample_count_description
      | sample_when_clause
      )
    ;

sample_fraction_description : fraction_description+=float_literal (',' fraction_description+=float_literal)* ;

sample_count_description : count_description+=integer_literal (',' count_description+=integer_literal)* ;

sample_when_clause
    : (WHEN logical_expr THEN (sample_fraction_description|sample_count_description) )+
      (ELSE (sample_fraction_description|sample_count_description) )?
      END
    ;

/* EXPAND ON clause */
expand_on_clause
    : EXPAND ON expand_expr=scalar_expr AS? expand_column_alias=alias_name
      ( BY (interval_literal | ANCHOR PERIOD? anchor_name (AT time_literal)? ) )?
      ( FOR period_expression=scalar_expr)?
    ;

order_by_clause : ORDER BY order_by_spec_full (',' order_by_spec_full)* ;

order_by_spec_full : scalar_expr (ASC|DESC)? (NULLS (FIRST|LAST) )? ;

order_by_spec_asc_desc_only : scalar_expr (ASC|DESC)? ;

with_check_option : WITH CHECK OPTION ;

/*********************
    Other Expressions
*/
logical_expr
    : scalar_expr op=comparison_operator scalar_expr # ScalarCompareScalar
    | scalar_expr op=comparison_operator quantifier elements_list attribute_modification* # ScalarComparelist
    | tuple op=comparison_operator quantifier elements_list attribute_modification* # TupleComparelist
    | scalar_expr (NOT|'^')? BETWEEN scalar_expr AND scalar_expr # Between
    | scalar_expr IS? (NOT|'^')? IN quantifier? scalar_expr_list attribute_modification*  # ScalarInList
    | scalar_expr IS? (NOT|'^')? IN quantifier? subquery attribute_modification* # ScalarInSubquery
    | scalar_expr IS? (NOT|'^')? IN quantifier? scalar_expr attribute_modification* # ScalarInScalar // same as = and <>
    | tuple IS? (NOT|'^')? IN quantifier? subquery attribute_modification* # TupleInSubquery
    | scalar_expr (NOT|'^')? LIKE scalar_expr (ESCAPE scalar_expr)? # ScalarLikeScalar
    | scalar_expr
      (NOT|'^')? LIKE quantifier elements_list attribute_modification* (ESCAPE scalar_expr)? attribute_modification* # ScalarLikeList
    | tuple (NOT|'^')? LIKE quantifier elements_list attribute_modification* (ESCAPE scalar_expr)? attribute_modification* # TupleLikeList
    | (NOT|'^')? EXISTS subquery # Exists
    | (NOT|'^') logical_expr # LogicalNot
    | logical_expr AND logical_expr # LogicalAnd
    | logical_expr OR logical_expr # LogicalOr
    | scalar_expr IS (NOT|'^')? NULL # NullCheck
    | scalar_expr (NOT|'^')? CONTAINS scalar_expr #PeriodContains
    | scalar_expr (NOT|'^')? EQUALS scalar_expr #PeriodEquals
    | scalar_expr (NOT|'^')? MEETS scalar_expr #PeriodMeets
    | scalar_expr (NOT|'^')? PRECEDES scalar_expr #PeriodPrecedes
    | scalar_expr (NOT|'^')? SUCCEEDS scalar_expr #PeriodSucceeds
    | scalar_expr IMMEDIATELY PRECEDES scalar_expr #PeriodImmediatelyPrecedes
    | scalar_expr IMMEDIATELY SUCCEEDS scalar_expr #PeriodImmediatelySucceeds
    | (scalar_expr|'('scalar_expr ',' scalar_expr ')'|subquery)
      OVERLAPS (scalar_expr|'('scalar_expr ',' scalar_expr ')'|subquery) #PeriodOverlaps
    | '(' logical_expr ')' # ParenthesizedLogicalExpr
    ;

scalar_expr
    : literal # LiteralExpr
    | op=('+'|'-') scalar_expr # UnaryPlusMinus
    | <assoc=right> scalar_expr '**' scalar_expr # Exponentiation
    | scalar_expr op=('*'|'/') scalar_expr # MultDiv
    | <assoc=right> scalar_expr MOD scalar_expr # Modulo
    | scalar_expr op=('+'|'-') scalar_expr # AddSub
    | scalar_expr ('||'|BROKEN_CONCATENATE) scalar_expr # Concatenation
    | case_expr # CaseExpr
    | partitioning_expr # PartitioningExpr
    | column_name # ColumnName
    | scalar_expr at_timezone # DateTimeExpr // in fact this is date_time_term. Other forms (derived from arithmetic expr): interval_expression + date_time_term; date_time_expression {+|-} interval_term
    | scalar_expr '-' scalar_expr interval_expr_start_end_phrase #IntervalExpr // Other forms (derived from arithmetic expr): interval_term ; interval_expression {+|-} interval_term
    | interval_expr_parenthesized #IntervalExprParenthesized
    | scalar_expr attribute_modification+ #AttributeModification
    | scalar_expr P_INTERSECT scalar_expr #PeriodIntersect
    | scalar_expr (LDIFF|RDIFF) scalar_expr #PeriodDiff
    | variable_reference #VariableReference
    | cursor_variable_reference #CursorVariableReference
    | macro_parameter_reference #MacroParameterReference
    // JSON entity reference expressions
    | scalar_expr '.' name=unqualified_name # JsonObjectMember
    | scalar_expr '.' '*' # JsonAllObjectMembers
    // JsonArrayElementReference is covered by ArrayElementReference alternative in this rule
    | scalar_expr '[' '*' ']' # JsonAllElements
    | scalar_expr '[' from_index=integer_literal ':' to_index=integer_literal (':' step_size=integer_literal )? ']' # JsonSlice
    | scalar_expr '.' '.' name=unqualified_name # JsonRecursiveDescendObjectMember
    | scalar_expr '.' '.' '*' # JsonRecursiveDescendAllObjectMembers
    | scalar_expr '.' '.' '['  index_value+=integer_literal ']' # JsonRecursiveDescendArrayElementReference
    | scalar_expr '.' '.' '[' '*' ']' # JsonRecursiveDescendAllArrayElements
    | scalar_expr '.' '.' '[' from_index=integer_literal ':' to_index=integer_literal (':' step_size=integer_literal )? ']' # JsonRecursiveDescendSlice
    // JSON methods
    | scalar_expr '.' ASBSON '(' validation_specification=char_string_literal? ')' # JsonAsBson
    | scalar_expr '.' ASBSONTEXT '('  ')' # JsonAsBsonText
    | scalar_expr '.' COMBINE '(' scalar_expr (',' array_or_object=char_string_literal)?  ')' # JsonCombine
    | scalar_expr '.' EXISTVALUE '(' json_path_expr=scalar_expr  ')' # JsonExistValue
    | scalar_expr '.' JSONEXTRACT '(' json_path_expr=scalar_expr  ')' # JsonExtract
    | scalar_expr '.' JSONEXTRACTVALUE '(' json_path_expr=scalar_expr  ')' # JsonExtractValue
    | scalar_expr '.' JSONEXTRACTLARGEVALUE '(' json_path_expr=scalar_expr  ')' # JsonExtractLargeValue
    | scalar_expr '.' KEYCOUNT '(' depth=integer_literal  ')' # JsonKeycount
    | scalar_expr '.' METADATA '(' ')' # JsonMetadata
    | scalar_expr '.' STORAGE_SIZE '(' storage_format=char_string_literal ')' # JsonMetadata
    // XML methods
    | scalar_expr '.' CREATESCHEMABASEDXML '(' schema=scalar_expr ')' # XMLCreateSchemaBasedXML
    | scalar_expr '.' CREATENONSCHEMABASEDXML '(' ')' # XMLCreateNonSchemaBasedXML
    | scalar_expr '.' EXISTSNODE '(' (filter=scalar_expr ',')? query=scalar_expr ',' nsmap=scalar_expr ')' # XMLExistNode
    | scalar_expr '.' ISCONTENT '(' ')' # XMLIsContent
    | scalar_expr '.' ISDOCUMENT '(' ')' # XMLIsDocument
    | scalar_expr '.' ISSCHEMAVALID '(' schema=scalar_expr ',' elem_decl=scalar_expr ',' ns=scalar_expr ')' # XMLIsSchemaValid
    | scalar_expr '.' ISSCHEMAVALIDATED '(' ')' # XMLIsSchemaValidated
    | scalar_expr '.' XMLEXTRACT '(' (filter=scalar_expr ',')? query=scalar_expr ',' nsmap=scalar_expr ')' # XMLExtract
    | scalar_expr '.' XMLTRANSFORM '(' xsl=scalar_expr ',' parammap=scalar_expr ')' # XMLTransform
    // UDT Constructors
    | NEW sysudtlib? udt_name '(' (scalar_expr (',' scalar_expr)* )? ')' # UDTConstructor
    | NEW JSON '(' json_data=scalar_expr (',' (LATIN|UNICODE|BSON|UBJSON))?  ')' # JSONConstructor
    | NEW VARIANT_TYPE '(' scalar_expr (AS? alias_name)? (',' scalar_expr (AS? alias_name)? )* ')' # VariantTypeConstructor
    | NEW XML '(' scalar_expr ')' # XMLConstructor
    // Array operators and methods
    | scalar_expr '[' index_value+=integer_literal (',' index_value+=integer_literal)* ']' # ArrayElementReference
    | scalar_expr '.' CARDINALITY '(' ( array_scope_reference )? ')' # ArrayCardinality
    | scalar_expr '.' ARRAY_CONCAT '(' other_array=scalar_expr (',' array_scope_reference )? ')' # ArrayCardinality
    | scalar_expr '.' (ARRAY_GT|ARRAY_GE|ARRAY_LT|ARRAY_LE|ARRAY_EQ|ARRAY_NE) '(' other_array=scalar_expr (',' array_scope_reference )? ')' # ArrayComparison
    | scalar_expr '.' (ARRAY_ADD|ARRAY_SUB|ARRAY_MUL|ARRAY_DIV|ARRAY_MOD) '(' other_array=scalar_expr (',' array_scope_reference )? ')' # ArrayArithmetic
    | scalar_expr '.' (ARRAY_SUM|ARRAY_AVG|ARRAY_MAX|ARRAY_MIN) '(' ( array_scope_reference )? ')' # ArrayAggregation
    | scalar_expr '.' ARRAY_COUNT_DISTINCT '(' ( array_scope_reference (',' matching_expr=scalar_expr)? )? ')' # ArrayAggregation
    | scalar_expr '.' ARRAY_GET '(' array_index=scalar_expr ')' # ArrayGet
    | scalar_expr '.' ARRAY_COMPARE '(' other_expr=scalar_expr (',' array_scope_reference (',' nulls_equal_flag=integer_literal )? )? ')' # ArrayGet
    | scalar_expr '.' ARRAY_UPDATE '(' new_value=scalar_expr (',' (array_scope_reference|array_index=integer_literal) )? ')' # ArrayUpdate
    | scalar_expr '.' ARRAY_UPDATE_STRIDE '(' new_value=scalar_expr ',' stride=integer_literal (',' (array_scope_reference|array_index=integer_literal) )? ')' # ArrayUpdateStride
    | scalar_expr '.' (OEXISTS|OPRIOR|ONEXT) '(' scalar_expr ')' # ArrayOmethodWithArg
    | scalar_expr '.' (OCOUNT|OLIMIT|OFIRST|OLAST|ODELETE) '(' ')' # ArrayOmethodWithoudArgs
    | scalar_expr '.' OEXTEND '(' ')' # ArrayOextend
    | scalar_expr '.' OTRIM '(' num_remove=integer_literal? ')' # ArrayOtrim
    // UDT method invocation
    | scalar_expr '.' method_name '(' (scalar_expr (',' scalar_expr)* )? ')' # UDTMethodInvocation
    //
    | scalar_expr teradata_type_conversion # DataTypeConversion
    | function_invocation # FunctionInvocation
    | '(' scalar_expr ')' # Parenthesized
    | subquery # ScalarSubquery
    ;

tuple
    : '(' tuple_attribute (',' tuple_attribute )+ ')'
    ;

tuple_attribute
    : scalar_expr (AS? alias_name)?
    | '(' scalar_expr AS? alias_name ')'
    ;

case_expr
    : valued_case_expr
    | searched_case_expr
    | coalesce_expr
    | nullif_expr
    ;

valued_case_expr
    : CASE scalar_expr
      (WHEN scalar_expr THEN scalar_expr)+
      (ELSE scalar_expr)?
      END
    ;

searched_case_expr
    : CASE
      (WHEN logical_expr THEN scalar_expr)+
      (ELSE scalar_expr)?
      END
    ;

coalesce_expr : COALESCE scalar_expr_list_comma_separated ;

nullif_expr : NULLIF '(' scalar_expr ',' scalar_expr ')' ;

// The following rules are used to hack over left-recursive rules and nested parenthesis
interval_expr_base
    : scalar_expr '-' scalar_expr
    | '(' scalar_expr '-' scalar_expr ')'
    ;

interval_expr_parenthesized
    : '(' interval_expr_base ')' interval_expr_start_end_phrase
    ;

interval_expr_start_end_phrase
    : (YEAR|MONTH|DAY|HOUR|MINUTE|SECOND)
      ( '(' precision=integer_literal (',' fractional_seconds_precision=integer_literal)? ')' )?
      (TO (MONTH|HOUR|MINUTE|SECOND) ( '(' fractional_seconds_precision=integer_literal ')' )? )?
    ;

function_invocation
    : aggregate_function
    | analytic_function
    | arithmetic_function
    | array_function
    | attribute_function
    | byte_function
    | builtin_function
    | calendar_function
    | comparison_function
    | compression_function
    | conversion_function
    | date_function
    | hash_function
    | lob_function
    | map_function
    | nvl_funtion
    | period_function
    | regexp_function
    | string_function
    | json_function
    | xml_function
    | other_function
    ;

aggregate_function
    : (AVERAGE|AVG|AVE|KURTOSIS|MAXIMUM|MAX|MIN|MINIMUM|SKEW|STDDEV_POP|STDDEV_SAMP|SUM|VAR_POP|VAR_SAMP)
      '(' (DISTINCT|ALL)? scalar_expr ')' # AggOneArg
    | ( CORR|COVAR_POP|COVAR_SAMP|REGR_AVGX|REGR_AVGY
      | REGR_COUNT|REGR_INTERCEPT|REGR_R2|REGR_SLOPE
      | REGR_SXX|REGR_SXY|REGR_SYY
      ) '(' scalar_expr ',' scalar_expr ')' # AggTwoArgs
    | COUNT '(' ('*'|(DISTINCT|UNIQUE|ALL)? scalar_expr) ')' # AggCount
    | GROUPING '(' scalar_expr ')' # Grouping
    | LISTAGG '(' scalar_expr ')' # ListAgg
    ;


analytic_function
    : (AVERAGE|AVG|AVE|MAXIMUM|MAX|MIN|MINIMUM|STDDEV_POP|STDDEV_SAMP|SUM|VAR_POP|VAR_SAMP)
      '(' value_expr=scalar_expr ')' window_spec
    | ( CORR|COVAR_POP|COVAR_SAMP|REGR_AVGX|REGR_AVGY
      | REGR_COUNT|REGR_INTERCEPT|REGR_R2|REGR_SLOPE
      | REGR_SXX|REGR_SXY|REGR_SYY
      ) '(' scalar_expr ',' scalar_expr ')' window_spec
    | COUNT '(' ('*'| value_expr=scalar_expr) ')' window_spec
    | CSUM '(' value_expr=scalar_expr (',' order_by_spec_asc_desc_only )+ ')'
    | (CUME_DIST|DENSE_RANK|PERCENT_RANK) '(' ')' window_spec_without_rows
    | (FIRST_VALUE|LAST_VALUE) '(' value_expr=scalar_expr  ignore_respect_nulls? ')' window_spec
    | (LAG|LEAD) '(' value_expr=scalar_expr ignore_respect_nulls? (',' offset_value=scalar_expr (',' default_value_expr=scalar_expr)? )? ')'
      ignore_respect_nulls? window_spec_without_rows // ANSI - ignore_respect_nulls after ')', Teradata - ignore_respect_nulls after value_expr
    | (MAVG|MDIFF|MSUM) '(' value_expr=scalar_expr ',' width=integer_literal (',' order_by_spec_asc_desc_only )+ ')'
    | MEDIAN '(' value_expr=scalar_expr ')'
    | MLINREG '(' value_expr=scalar_expr ',' width=integer_literal ',' order_by_spec_asc_desc_only ')'
    | (PERCENTILE_CONT|PERCENTILE_DISC) '(' value_expr=scalar_expr ')'
      WITHIN GROUP '(' ORDER BY order_by_spec_full (',' order_by_spec_full)* ')'
    | QUANTILE '(' quantile_literal=integer_literal (',' order_by_spec_asc_desc_only )+ ')'
    | RANK '(' ')' window_spec_with_ties  // ANSI
    | RANK '(' order_by_spec_asc_desc_only (',' order_by_spec_asc_desc_only)* ')' //Teradata
    | ROW_NUMBER '(' ')' window_spec_without_rows
    | LISTAGG '(' scalar_expr ')' window_spec
    ;

arithmetic_function
    : ( ABS|DEGREES|RADIANS|EXP|SQRT
      | COSH|SINH|TANH|ACOSH|ASINH|ATANH
      | LN|LOG
      | NULLIFZERO|ZEROIFNULL
      | COS|SIN|TAN|ACOS|ASIN|ATAN
      ) '(' scalar_expr ')'
    | td_sysfnlib? (CEILING|CEIL|FLOOR|SIGN) '(' scalar_expr ')'
    | (MOD|RANDOM|ATAN2) '(' scalar_expr ',' scalar_expr ')'
    | syslib? POWER '(' scalar_expr ',' scalar_expr ')'
    | td_sysfnlib? (ROUND|TRUNC) '(' scalar_expr (',' scalar_expr)? ')'
    | WIDTH BUCKET '(' scalar_expr ',' scalar_expr ',' scalar_expr ',' scalar_expr ')'
    ;

array_function
    : td_sysfnlib? ARRAY_AGG '(' element_value_expr=scalar_expr (ORDER BY scalar_expr (ASC|DESC)? )? ',' array_expr=scalar_expr ')'
    | CARDINALITY '(' array_expr=scalar_expr (',' array_scope_reference )* ')'
    | ARRAY_CONCAT '(' array_expr_1=scalar_expr ',' array_expr_2=scalar_expr (',' array_scope_reference )* ')'
    | (ARRAY_GT|ARRAY_GE|ARRAY_LT|ARRAY_LE|ARRAY_EQ|ARRAY_NE) '(' array_expr_1=scalar_expr ',' array_expr_2=scalar_expr (',' array_scope_reference )* ')'
    | (ARRAY_ADD|ARRAY_SUB|ARRAY_MUL|ARRAY_DIV|ARRAY_MOD) '(' array_expr_1=scalar_expr ',' array_expr_2=scalar_expr (',' array_scope_reference )* ')'
    | (ARRAY_SUM|ARRAY_AVG|ARRAY_MAX|ARRAY_MIN) '(' array_expr=scalar_expr (',' array_scope_reference )* ')'
    | ARRAY_COUNT_DISTINCT '(' array_expr=scalar_expr ((',' array_scope_reference )* ',' matching_expr=scalar_expr|(',' array_scope_reference )+ )? ')'
    | ARRAY_GET '(' array_expr=scalar_expr ',' array_index=scalar_expr ')'
    | ARRAY_COMPARE '(' array_expr_1=scalar_expr ',' array_expr_2=scalar_expr ((',' array_scope_reference )+ ',' nulls_equal_flag=integer_literal)? ')'
    | ARRAY_UPDATE '(' array_expr=scalar_expr ',' new_value=scalar_expr ((',' array_scope_reference )+ |',' array_index=scalar_expr)? ')'
    | ARRAY_UPDATE_STRIDE '(' array_expr=scalar_expr ',' new_value=scalar_expr ',' stride=integer_literal ((',' array_scope_reference )+ |',' array_index=scalar_expr)? ')'
    | td_sysfnlib? (OEXISTS|OPRIOR|ONEXT) '(' array_expr=scalar_expr ',' index_value_or_bound=scalar_expr ')'
    | td_sysfnlib? (OCOUNT|OFIRST|OLAST|ODELETE) '(' array_expr=scalar_expr ')'
    | td_sysfnlib? OEXTEND '(' array_expr=scalar_expr (',' num_spaces=scalar_expr (',' index_value_or_bound=scalar_expr)? )? ')'
    | td_sysfnlib? OTRIM '(' array_expr=scalar_expr (',' num_elements=scalar_expr)? ')'
    ;

attribute_function
    : BIT_LENGTH '(' scalar_expr (',' character_set_name=literal)? ')'
    | (BYTE|BYTES) '(' scalar_expr ')'
    | (CHARACTER_LENGTH|CHAR_LENGTH|CHAR|CHARS|CHARACTER|CHARACTERS|MCHARACTERS) '(' scalar_expr ')'
    | DEFAULT ('(' column_name ')')?
    | (FORMAT|TITLE|TYPE) '(' scalar_expr ')'
    | OCTET_LENGTH '(' scalar_expr (',' character_set_name=literal)? ')'
    ;

byte_function
    : td_sysfnlib? BITNOT '(' scalar_expr ')'
    | td_sysfnlib? ( BITAND|BITOR|BITXOR
                   | GETBIT|ROTATELEFT|ROTATERIGHT
                   | SHIFTLEFT|SHIFTRIGHT
                   ) '(' scalar_expr ',' scalar_expr ')'
    | td_sysfnlib? COUNTSET '(' scalar_expr (',' scalar_expr)? ')'
    | td_sysfnlib? SETBIT '(' scalar_expr ',' scalar_expr (',' scalar_expr)? ')'
    | td_sysfnlib? SUBBITSTR '(' scalar_expr ',' scalar_expr ',' scalar_expr ')'
    | td_sysfnlib? TO_BYTE '(' scalar_expr ')'
    ;

builtin_function
    : ACCOUNT
    | (CURRENT_DATE ('(' ')')?|CURDATE '(' ')') at_timezone?
    | CURRENT_ROLE
    | (CURRENT_TIME type_precision?|CURTIME '(' ')') at_timezone?
    | CURRENT_TIMESTAMP type_precision? at_timezone?
    | CURRENT_USER
    | DATABASE
    | DATE at_timezone?
    | NOW '(' ')'
    | PROFILE
    | ROLE
    | SESSION
    | TEMPORAL_DATE
    | TEMPORAL_TIMESTAMP type_precision?
    | TIME at_timezone?
    | USER
    ;

calendar_function
    : syslib? ( TD_DAY_OF_CALENDAR
              | TD_DAY_OF_MONTH
              | TD_DAY_OF_WEEK
              | TD_DAY_OF_YEAR
              | TD_MONTH_OF_CALENDAR
              | TD_MONTH_OF_QUARTER
              | TD_MONTH_OF_YEAR
              | TD_QUARTER_OF_CALENDAR
              | TD_QUARTER_OF_YEAR
              | TD_WEEK_OF_CALENDAR
              | TD_WEEK_OF_MONTH
              | TD_WEEK_OF_YEAR
              | TD_WEEKDAY_OF_MONTH
              | TD_YEAR_OF_CALENDAR
              ) '(' scalar_expr ')'
    | td_sysfnlib? ( DAYNUMBER_OF_CALENDAR
                   | DAYNUMBER_OF_MONTH
                   | DAYNUMBER_OF_WEEK
                   | DAYNUMBER_OF_YEAR
                   | DAYOCCURRENCE_OF_MONTH
                   | MONTHNUMBER_OF_CALENDAR
                   | MONTHNUMBER_OF_QUARTER
                   | MONTHNUMBER_OF_YEAR
                   | QUARTERNUMBER_OF_CALENDAR
                   | QUARTERNUMBER_OF_YEAR
                   | WEEKNUMBER_OF_CALENDAR
                   | WEEKNUMBER_OF_MONTH
                   | WEEKNUMBER_OF_QUARTER
                   | WEEKNUMBER_OF_YEAR
                   | YEARNUMBER_OF_CALENDAR
                   ) '(' scalar_expr
      ','? (calendar_for_session=NULL|calendar_name=char_string_literal)? ')'
    ;

comparison_function
    : td_sysfnlib? DECODE '(' expr=scalar_expr
      ',' search_result=scalar_expr (',' search_result=scalar_expr) default_result=scalar_expr ')'
    | td_sysfnlib? (GREATEST|LEAST) '(' scalar_expr  (',' scalar_expr)+ ')'
    ;

compression_function
    : td_sysfnlib?
      ( CAMSET|CAMSET_L|DECAMSET|DECAMSET_L|JSON_COMPRESS|JSON_DECOMPRESS
      | LZCOMP|LZCOMP_L|LZDECOMP|LZDECOMP_L|TD_LZ_COMPRESS|TD_LZ_DECOMPRESS|TS_COMPRESS|TS_DECOMPRESS
      | TRANSUNICODETOUTF8|TRANSUTF8TOUNICODE )
      '(' scalar_expr ')'
    ;
conversion_function
    : CAST '(' expr=scalar_expr AS (data_type data_type_attribute*|data_type_attribute+ ) ')'
    | TRYCAST '(' in_string=scalar_expr AS data_type ')'
    | td_sysfnlib? TO_BYTES '(' in_string=scalar_expr ',' in_encoding=char_string_literal ')'
    | td_sysfnlib? FROM_BYTES '(' in_string=scalar_expr ',' out_encoding=char_string_literal ')'
    | td_sysfnlib? TO_NUMBER '(' string_expr=scalar_expr (',' format_arg=char_string_literal (',' (nls_param=char_string_literal|NULL) )? )? ')'
    | td_sysfnlib? TO_CHAR '(' expr=scalar_expr (',' format_arg=char_string_literal (',' (nls_param=char_string_literal|NULL) )? )? ')'
    | td_sysfnlib? TO_DATE '(' in_string=scalar_expr ',' (format_arg=char_string_literal|NULL) ')'
    | td_sysfnlib? (TO_TIMESTAMP|TO_TIMESTAMP_TZ) '(' expr=scalar_expr (',' (format_arg=char_string_literal|NULL) )? ')'
    | td_sysfnlib? (TO_YMINTERVAL|TO_DSINTERVAL) '(' scalar_expr ')'
    | td_sysfnlib? (NUMTODSINTERVAL|NUMTOYMINTERVAL) '(' numeric_value=scalar_expr ',' interval_unit=scalar_expr ')'
    ;

date_function
    : (YEAR|MONTH|WEEK|DAYOFMONTH|HOUR|MINUTE|SECOND) '(' scalar_expr ')'
    | td_sysfnlib? LAST_DAY '(' scalar_expr ')'
    | td_sysfnlib? NEXT_DAY '(' scalar_expr ',' day_value=char_string_literal ')'
    | td_sysfnlib? MONTHS_BETWEEN '(' scalar_expr ',' scalar_expr ')'
    | td_sysfnlib? (ADD_MONTHS|OADD_MONTHS) '(' scalar_expr ',' scalar_expr ')'
    | EXTRACT '(' (YEAR|MONTH|DAY|HOUR|MINUTE|SECOND|TIMEZONE_HOUR|TIMEZONE_MINUTE) FROM scalar_expr ')'
    ; // TRUNC and ROUND are in arithmetic_function rule

hash_function
    : (HASHAMP|HASHBAKAMP) '(' ')'
    | (HASHAMP|HASHBAKAMP) '(' hash_bucket_number_expr ')'
    | HASHBUCKET '(' scalar_expr ')'
    | HASHROW '(' ')'
    | HASHROW '(' scalar_expr (',' scalar_expr)* ')'
    ;

lob_function : td_sysfnlib? (EMPTY_BLOB|EMPTY_CLOB) '(' ')' ;

map_function
    : syslib? CONTIGUOUSMAPAMPS '(' map_name_string=char_string_literal ')'
    | syslib? SPARSEMAPAMPS '(' database_name_string=char_string_literal
                            ',' object_name_string=char_string_literal
                            ',' object_kind_string=char_string_literal
                            ',' map_slot_string=char_string_literal
                            ',' number_of_primary_amps_string=char_string_literal
                            ',' colocation_name_string=char_string_literal
                            ')'
    | syslib? SPARSETABLEAMPS '(' database_name_string=char_string_literal ',' object_name_string=char_string_literal ')'
    ;

nvl_funtion
    : td_sysfnlib? NVL '(' scalar_expr ',' scalar_expr ')'
    | td_sysfnlib? NVL2 '(' scalar_expr ',' scalar_expr ',' scalar_expr ')'
    ;

period_function
    : BEGIN '(' scalar_expr ')'
    | END '(' scalar_expr ')'
    | END '(' scalar_expr ')' IS NOT? (UNTIL_CHANGED|UNTIL_CLOSED)
    | INTERVAL '(' scalar_expr ')' interval_period_spec
    | LAST '(' scalar_expr ')'
    | NEXT '(' scalar_expr ')'
    | PRIOR '(' scalar_expr ')'
    ;

regexp_function
    : td_sysfnlib? REGEXP_SUBSTR
      '(' source_string=scalar_expr ',' regexp_string=scalar_expr
      (',' position_arg=scalar_expr ',' occurrence_arg=scalar_expr ',' match_arg=scalar_expr)? ')'
    | td_sysfnlib? REGEXP_REPLACE
      '(' source_string=scalar_expr ',' regexp_string=scalar_expr
      (',' replace_string=scalar_expr ',' position_arg=scalar_expr ',' occurrence_arg=scalar_expr ',' match_arg=scalar_expr)? ')'
    | td_sysfnlib? REGEXP_INSTR
      '(' source_string=scalar_expr ',' regexp_string=scalar_expr
      (',' position_arg=scalar_expr ',' occurrence_arg=scalar_expr ',' return_opt=scalar_expr ',' match_arg=scalar_expr)? ')'
    | td_sysfnlib? REGEXP_SIMILAR
      '(' source_string=scalar_expr ',' regexp_string=scalar_expr (',' match_arg=scalar_expr)? ')'
    | td_sysfnlib? REGEXP_SPLIT_TO_TABLE
      '(' in_k=scalar_expr ',' source_string=scalar_expr ',' regexp_string=scalar_expr ',' match_arg=scalar_expr ')'
    ;

string_function
    : td_sysfnlib? (ASCII|CHR|INITCAP|LENGTH|REVERSE) '(' scalar_expr ')'
    | (CHAR2HEXINT|SOUNDEX|STRING_CS|VARGRAPHIC) '(' scalar_expr ')'
    | CONCAT '(' scalar_expr ',' scalar_expr (',' scalar_expr)* ')'
    | td_sysfnlib? CSV '(' NEW VARIANT TYPE '(' scalar_expr (',' scalar_expr)* ')' ','
      delim_string_value=char_string_literal ','
      quote_string_value=char_string_literal ')'
    | td_sysfnlib? CSVLD '(' data_string_value=scalar_expr ','
      delim_string_value=char_string_literal ','
      quote_string_value=char_string_literal ')'
    | td_sysfnlib? EDITDISTANCE '(' scalar_expr ',' scalar_expr
      (',' ci=integer_literal ',' cd=integer_literal ',' cs=integer_literal ',' ct=integer_literal)? ')'
    | INDEX '(' scalar_expr ',' scalar_expr ')'
    | td_sysfnlib? INSTR '(' source_string=scalar_expr ',' search_string=scalar_expr
      (',' position=scalar_expr (',' occurrence=scalar_expr)? )? ')'
    | td_sysfnlib? (LEFT|RIGHT) '(' source_string=scalar_expr ',' length=scalar_expr ')'
    | LOCATE '(' scalar_expr ',' scalar_expr (',' scalar_expr )? ')'
    | (LOWER|UPPER|UCASE) '(' scalar_expr ')'
    | td_sysfnlib? (LPAD|RPAD) '(' source_string=scalar_expr ',' length=scalar_expr (',' fill_string=scalar_expr )? ')'
    | td_sysfnlib? (LTRIM|RTRIM) '(' scalar_expr (',' scalar_expr )? ')'
    | td_sysfnlib? NGRAM '(' scalar_expr ',' scalar_expr ',' length=scalar_expr (',' position=scalar_expr )? ')'
    | td_sysfnlib? NVP '(' in_string=scalar_expr ',' name_to_search=scalar_expr
      (',' name_delimiters=scalar_expr (',' value_delimiters=scalar_expr (',' occurrence=scalar_expr)? )? )? ')'
    | td_sysfnlib? OREPLACE '(' source_string=scalar_expr ',' search_string=scalar_expr (',' replace_string=scalar_expr )? ')'
    | td_sysfnlib? OTRANSLATE '(' source_string=scalar_expr ',' from_string=scalar_expr ',' to_string=scalar_expr ')'
    | POSITION '(' scalar_expr IN scalar_expr ')'
    | td_sysfnlib? STRTOK '(' in_string=scalar_expr (',' delimiter=scalar_expr (',' token_num=scalar_expr)? )? ')'
    | td_sysfnlib? STRTOK_SPLIT_TO_TABLE '(' in_k=scalar_expr ',' in_string=scalar_expr ',' delimiters=scalar_expr ')'
      RETURNS '(' out_k=column_spec ',' out_token_num=column_spec ',' out_token=column_spec ')'
    | SUBSTRING '(' scalar_expr FROM scalar_expr ((FOR|',') scalar_expr)? ')' //ANSI; yes, (FOR|',') is valid
    | (SUBSTRING|SUBSTR) '(' scalar_expr ',' scalar_expr ((FOR|',') scalar_expr)? ')' //Teradata; yes, (FOR|',') is valid
    | (TRANSLATE|TRANSLATE_CHK) '(' string_expr=scalar_expr
      USING translation_mapping
      (WITH ERROR)? ')'
    | TRIM '(' ( (BOTH|TRAILING|LEADING) (trim_expr=scalar_expr)? FROM)? string_expr=scalar_expr ')'
    ;

json_function
    : td_sysfnlib? ARRAY_TO_JSON '(' array_expr=scalar_expr ')' returns_clause?
    | td_sysfnlib? BSON_CHECK '(' bson_data=scalar_expr (',' check_type=char_string_literal /*('STRICT'|'LAX')*/)? ')'
    | td_sysfnlib? DATASIZE '(' scalar_expr ')'
    | td_sysfnlib? GEOJSONFROMGEOM '(' geom_expr=scalar_expr (',' precision=integer_literal)? ')' returns_clause?
    | td_sysfnlib? GEOMFROMGEOJSON '(' geojson_expr=scalar_expr ',' asrid=integer_literal')'
    | td_sysfnlib? JSON_CHECK '(' json_string=scalar_expr ')'
    | JSONGETVALUE '(' json_object=scalar_expr ',' json_path=scalar_expr AS data_type ')'
    | JSONMETADATA '(' json_object=scalar_expr ')'
    | td_sysfnlib? NVP2JSON '(' nvp_string=scalar_expr (',' name_delimiters=char_string_literal ',' value_delimiters=char_string_literal (',' ignore_characters=char_string_literal)? )? ')'
    | JSON_AGG '(' json_param_spec (',' json_param_spec)* ')' returns_clause?
    | JSON_COMPOSE '(' json_param_spec (',' json_param_spec)* ')' returns_clause?
    ;

xml_function
    : CREATEXML '(' xml_data=scalar_expr ')'
    | td_sysfnlib? DATASIZE '(' scalar_expr ')'
    | XMLQUERY '(' xquery_expr=char_string_literal  xml_query_argument? xml_returning_spec? xml_query_on_empty? ')'
    | XMLSERIALIZE '('
      (DOCUMENT|CONTENT) xml_value_exp=scalar_expr (AS data_type)?
      xml_encoding?
      (WITH NO? BOM)?
      (VERSION char_string_literal)?
      ((INCLUDING|EXCLUDING) XMLDECLARATION)?
      (NO INDENT|INDENT (SIZE '=' integer_literal) )?
      ')'
    | XMLDOCUMENT '(' xml_value_expr=scalar_expr xml_returning_spec? ')'
    | XMLELEMENT '(' NAME column_name (',' xml_namespace_declaration)?
      (',' xml_attribute_declaration)?
      (',' xml_value_declaration)?
      xml_returning_spec?
      ')'
    | XMLFOREST '(' xml_namespace_declaration?
      (xml_forest_element_spec (',' xml_forest_element_spec)* )?
      xml_content_option_spec? xml_returning_spec? ')'
    | XMLCONCAT '(' xml_value_expr=scalar_expr (',' xml_value_expr=scalar_expr)* xml_returning_spec? ')'
    | XMLCOMMENT '(' xml_comment=scalar_expr xml_returning_spec? ')'
    | XMLPI '(' NAME column_name (',' scalar_expr)? xml_returning_spec? ')'
    | XMLTEXT '(' xml_text=scalar_expr xml_returning_spec? ')'
    | XMLPARSE '(' (DOCUMENT|CONTENT) scalar_expr (PRESERVE|STRIP) WHITESPACE ')'
    | XMLVALIDATE '('
      (DOCUMENT|CONTENT|SEQUENCE)
      xml_value_expr=scalar_expr
      ACCORDING TO XMLSCHEMA
      VALUE xml_schema=scalar_expr
      (NAMESPACE xml_uri=char_string_literal|NO NAMESPACE)?
      (ELEMENT xml_valid_element_name=scalar_expr)? ')'
    | XMLAGG '(' xml_value_expr=scalar_expr (ORDER BY order_by_spec_full (',' order_by_spec_full)*)? xml_returning_spec? ')'
    | XMLSPLIT '(' doc_id=scalar_expr ',' source_doc=scalar_expr ',' split_size=scalar_expr ',' split_path=scalar_expr (',' replication_list+=scalar_expr)* ')'
    | td_sysxml XMLPUBLISH '(' query_string=scalar_expr ',' xslt=scalar_expr ',' xml_string=scalar_expr ',' result_code=scalar_expr ')'
    | td_sysxml XMLPUBLISH_STREAM '(' query_string=scalar_expr ',' xslt=scalar_expr ',' document_grouping_spec=scalar_expr ')'
    ;

other_function // covers UDF
    : function_name '(' scalar_expr (',' scalar_expr)* ')' (returns_clause|window_spec)?
    | function_name '(' ')' (returns_clause|window_spec)?
    ;

partitioning_expr
    : CASE_N '(' logical_expr (',' logical_expr)* (',' case_spec)? ')'
    | RANGE_N '(' scalar_expr BETWEEN range_expr (',' range_spec)? ')'
    ;

td_sysfnlib : ('TD_SYSFNLIB' '.') ;

td_sysxml : 'TD_SYSXML' '.' ;

syslib : 'SYSLIB' '.' ;

td_server_db : TD_SERVER_DB '.' ;

translation_mapping // skipped many cryptic translation mappings
    : LATIN_TO_UNICODE
    | UNICODE_TO_LATIN
    | LOCALE_TO_UNICODE
    | UNICODE_TO_LOCALE
    ;

attribute_modification : '(' attribute_modification_option (',' attribute_modification_option)* ')' ;

returns_clause : RETURNS data_type|RETURNS STYLE column_name ;

attribute_modification_option
    : teradata_type_conversion_data_attribute
    | casespecific_phrase
    | character_set_phrase
    | uppercase_phrase
    ;

teradata_type_conversion
    : '(' (teradata_type_conversion_data_attribute ',')*
      data_type (',' (teradata_type_conversion_data_attribute|character_set_phrase) )*
      ')'
   ;

teradata_type_conversion_data_attribute
    : format_phrase
    | named_phrase
    | title_phrase
    ;

case_spec : NO CASE ((','|OR) UNKNOWN)? | UNKNOWN ;

range_expr
    : range_expr_1
    | range_expr_2
    | range_list
    ;

range_list
    : (range_expr_3 | '*' (AND scalar_expr)? )
      (',' range_expr_3)* ',' range_expr_1
    ;

range_expr_1 : scalar_expr AND (scalar_expr|'*') (EACH scalar_expr)? ;

range_expr_2 : '*' AND (scalar_expr|'*') ;

range_expr_3 : scalar_expr (AND scalar_expr) (EACH scalar_expr)? ;

range_spec : NO RANGE ((','|OR) UNKNOWN)? | UNKNOWN ;

hash_bucket_number_expr
    : scalar_expr (MAP '=' sparcemap_name=char_string_literal COLOCATE USING '=' colocation_name=char_string_literal)?
    | scalar_expr MAP '=' contiguousmap_name=char_string_literal
    | MAP '=' contiguousmap_name=char_string_literal
    ;

window_spec : OVER '(' window_partition_by? window_order_by? window_rows? ')' ;

window_spec_without_rows : OVER '(' window_partition_by? window_order_by ')' ;

window_spec_with_ties : OVER '(' window_partition_by? window_order_by with_ties? ')' ;

window_partition_by : PARTITION BY scalar_expr (',' scalar_expr)* ;

window_order_by : ORDER BY order_by_spec_full (',' order_by_spec_full)* (RESET WHEN logical_expr)? ;

window_rows
    : ROWS
      ( UNBOUNDED PRECEDING
      | number_of_rows PRECEDING
      | CURRENT ROW
      | BETWEEN ( UNBOUNDED PRECEDING AND UNBOUNDED FOLLOWING
                | UNBOUNDED PRECEDING AND number_of_rows PRECEDING
                | UNBOUNDED PRECEDING AND CURRENT ROW
                | UNBOUNDED PRECEDING AND number_of_rows FOLLOWING
                | number_of_rows PRECEDING AND UNBOUNDED FOLLOWING
                | number_of_rows PRECEDING AND number_of_rows PRECEDING
                | number_of_rows PRECEDING AND CURRENT ROW
                | number_of_rows PRECEDING AND number_of_rows FOLLOWING
                | CURRENT ROW AND UNBOUNDED FOLLOWING
                | CURRENT ROW AND CURRENT ROW
                | CURRENT ROW AND number_of_rows FOLLOWING
                | number_of_rows FOLLOWING AND UNBOUNDED FOLLOWING
                | number_of_rows FOLLOWING AND number_of_rows FOLLOWING
                )
      )
   ;

json_param_spec : param=scalar_expr ('(' format_phrase ')' )? (AS alias_name)? ;

xml_query_argument
    : PASSING (BY VALUE)?
      (xml_query_context_item=column_name | xml_query_variable_spec (',' xml_query_variable_spec)* )
    ;

xml_query_variable_spec : xml_query_variable=column_name AS variable_name ;

xml_attribute_declaration : XMLATTRIBUTES '(' xml_attribute_spec (',' xml_attribute_spec)* ')' ;

xml_attribute_spec : xml_attribute_value=column_name (AS xml_attribute_name=alias_name)? ;

xml_forest_element_spec : xml_forest_element_value=column_name (AS xml_forest_element_name=alias_name)? ;

xml_value_declaration : xml_value_expr=scalar_expr xml_content_option_spec? ;

xml_namespace_declaration : XMLNAMESPACES '(' xml_namespace_spec (',' xml_namespace_spec)* ')' ;

xml_namespace_spec
    : xml_namespace_uri=char_string_literal AS xml_namespace_prefix=alias_name
    | DEFAULT xml_namespace_uri=char_string_literal
    | NO DEFAULT
    ;

xml_columns_spec
    : COLUMNS (column_name FOR ORDINALITY|xml_regular_column_definition)
      (',' (column_name FOR ORDINALITY|xml_regular_column_definition) )*
    ;

xml_regular_column_definition : name=unqualified_name data_type (PATH char_string_literal)? (DEFAULT literal)? ;

xml_encoding : ENCODING xml_encoding_name=char_string_literal ;

xml_query_on_empty : (NULL|EMPTY) ON EMPTY ;

xml_returning_spec : RETURNING (CONTENT|SEQUENCE) ;

xml_content_option_spec
    : OPTION ( (NULL|EMPTY|ABSENT|NIL) ON NULL
             | NIL ON NO CONTENT
             )
    ;

ignore_respect_nulls : (IGNORE|RESPECT) NULLS ;

number_of_rows : integer_literal ;

with_ties : WITH TIES (LOW|HIGH|AVG|DENSE) ;

pivot : PIVOT '(' pivot_spec ')' pivot_with_phrase? AS? alias_name column_list? ;

pivot_spec : pivot_agg_func_spec (',' pivot_agg_func_spec)* FOR pivot_for_phrase ;

pivot_with_phrase : WITH pivot_with_spec (',' pivot_with_spec)* ;

pivot_agg_func_spec : aggregate_function (AS? alias_name)? ;

pivot_for_phrase
    : column_name IN '(' pivot_expr_spec_scalar (',' pivot_expr_spec_scalar)* ')'
    | column_list IN '(' pivot_expr_spec_scalar (',' pivot_expr_spec_scalar)* ')'
    | column_list IN '(' pivot_expr_spec_list (',' pivot_expr_spec_list)* ')'
    | column_name IN subquery
    ;

pivot_with_spec : aggregate_function AS? alias_name ;

pivot_expr_spec_scalar : scalar_expr (AS? alias_name)? ;

pivot_expr_spec_list : scalar_expr_list_comma_separated AS? alias_name ;

unpivot : UNPIVOT ((INCLUDE|EXCLUDE) NULLS)? '(' unpivot_spec ')' AS? alias_name column_list? ;

unpivot_spec
    : column_name FOR column_name IN '(' unpivot_column_name_spec_single (',' unpivot_column_name_spec_single)* ')'
    | column_list FOR column_name IN '(' unpivot_column_name_spec_list (',' unpivot_column_name_spec_list)* ')'
    ;

unpivot_column_name_spec_single : column_name (AS? literal)? ;

unpivot_column_name_spec_list : column_list (AS? literal)? ;

at_timezone
    : AT (
           LOCAL
           //The data type of expression should be INTERVAL HOUR(2) TO MINUTE or it must be a data
           //type that can be implicitly converted to INTERVAL HOUR(2) TO MINUTE.
         | (TIME ZONE)? scalar_expr // TODO: make more specific?
         )
    ;

elements_list
    : (subquery | scalar_expr_list) ;

scalar_expr_list : '(' scalar_expr ((','|OR) scalar_expr)* ')' ;

scalar_expr_list_comma_separated : '(' scalar_expr (',' scalar_expr)* ')' ;

column_list : '(' column_name (',' column_name)* ')' ;

subquery : '(' with_request_modifier? query_expr ')' ;

column_spec : name=unqualified_name data_type ;

variable_reference : ':' variable_name ;

cursor_variable_reference : cursor_reference=variable_reference '.' name=unqualified_name ;

macro_parameter_reference : (':'|'@') parameter_name ;

array_scope_reference
    : array_lower_bounds=integer_literal ',' array_upper_bounds=integer_literal
    ;

/************
    Operators
*/
comparison_operator
    : '=' | EQ
    | '<>' | '^=' | NE | NOT'=' | '<' '>'
    | '<' | LT
    | '<=' | LE
    | '>' | GT
    | '>=' | GE
    ;

quantifier : ALL | ANY | SOME ;
