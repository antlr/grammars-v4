parser grammar TeradataSQLRequestModifiersParser;

import TeradataSQLExpressionsParser
     ;

options {
    tokenVocab=TeradataSQLLexer;
}

request_modifier
    : locking_request_modifier
    | with_request_modifier
    | using_request_modifier
    | explain_request_modifier
    ;

/***************************
    LOCKING request modifier
*/
locking_request_modifier : locking_spec+ ;

locking_spec
    : (LOCKING|LOCK)
      ( DATABASE? database_name
      | TABLE? table_name
      | VIEW? view_name=table_name
      | ROW
      )
      (FOR|IN)?
      lock_type
      MODE? NOWAIT?
    ;

lock_type
    : ACCESS
    | (EXCL|EXCLUSIVE)
    | SHARE
    | READ OVERRIDE?
    | WRITE
    | CHECKSUM
    | LOAD COMMITTED
    ;

/************************
    WITH request modifier
*/
with_request_modifier : WITH cte_spec (',' cte_spec)* ;

cte_spec : regular_cte_spec | recursive_cte_spec ;

regular_cte_spec : query_name=alias_name column_list? AS subquery ;

        /* A RECURSIVE named query that can refer to itself in the query definition. The named query list consists
        of at least one nonrecursive, or seed, statement and at least one recursive statement.
        Only SELECT, FROM, and WHERE are allowed in resursive statement (?).
        You cannot include NORMALIZE in a recursive statement of a recursive query.
        */
recursive_cte_spec
    : RECURSIVE query_name=alias_name column_list?
      AS '(' query_expr (UNION ALL query_term)+ ')' // note that query_term is used for recursive statement
    ;

/*************************
    USING request modifier
*/
using_request_modifier : USING '(' using_spec (',' using_spec)* ')' ;

using_spec : variable_name data_type data_type_attribute* (AS (DEFERRED (BY NAME)?|LOCATOR) )? ;

/***************************
    EXPLAIN request modifier
*/
explain_request_modifier
    : (STATIC|DYNAMIC)? EXPLAIN (IN XML NODDLTEXT? )?
    ;
