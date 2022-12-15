/*
ArangoDB grammar.
The MIT License (MIT).

Copyright (c) 2022, Michał Lorek.

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:
The above copyright notice and this permission notice shall be included in
all copies or substantial portions of the Software.
THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
THE SOFTWARE.
*/

parser grammar ArangoDbParser;

options { tokenVocab=ArangoDbLexer; }

arangodb_query
    : data_query EOF
    ;

data_query
    : data_access_query
    | data_modification_query
    ;

data_access_query
    : with_collection_list? for_op* return_expr
    | with_collection_list?
        FOR v=id_ (',' e=id_ (',' p=id_)? )?
        IN (numeric_literal ('..' numeric_literal)? )?
        (OUTBOUND | INBOUND | ANY) expr
        (GRAPH expr | collection_list) // collections support direction: ANY
        (PRUNE expr)?
        filter*
        options_?
        return_expr?
    ;

for_op
    : for_in
      collect*
      filter*
      sort*
      limit*
      window*
    ;

data_modification_query
    : with_collection_list? for_op*
         INSERT object_literal in_into collection options_?
        return_expr?
    | with_collection_list? for_op*
        UPDATE (expr WITH)? object_literal IN collection options_?
        return_expr?
    | with_collection_list? for_op*
        REPLACE (expr WITH)? object_literal IN collection options_?
         return_expr?
    | with_collection_list? for_op*
        (REMOVE expr IN collection options_?)+
        return_expr?
    | with_collection_list? for_op*
        UPSERT expr INSERT expr (UPDATE | REPLACE) expr IN collection options_?
        return_expr?
    ;

options_
    : OPTIONS object_literal
    ;

for_in
    : let_list? FOR id_ IN (expr | collection) search? let_list?
    ;

filter
    : let_list? FILTER expr let_list?
    ;

sort
    : let_list? SORT expr asc_desc? (',' expr asc_desc)* let_list?
    ;

limit
    : let_list? LIMIT numeric_literal (',' numeric_literal)? let_list?
    ;

search
    : SEARCH expr options_?
    ;

asc_desc
    : ASC
    | DESC
    ;

in_into
    : IN
    | INTO
    ;

return_expr
    : let_list? RETURN DISTINCT? expr
    ;

with_collection_list
    : WITH collection_list
    ;

collection_list
    : collection (',' collection)*
    ;

collection
    : id_
    | BIND_PARAMETER_COLL
    ;

collect
    : COLLECT expr (INTO expr)? options_?
    | COLLECT expr (INTO id_ KEEP id_)? options_?
    | COLLECT expr WITH COUNT INTO id_ options_?
    | COLLECT expr? aggregate_assign (INTO id_)? options_?
    | COLLECT WITH COUNT INTO id_ options_?
    ;

window
    : WINDOW object_literal aggregate_assign
    | WINDOW expr WITH object_literal aggregate_assign
    ;

aggregate_assign
    : AGGREGATE id_ ASSIGN expr
    ;

let_list
    : (LET expr_list)+
    ;

id_
    : ID
    ;

new_old
    : NEW
    | OLD
    ;

expr
    : literal
    | '(' (expr | data_access_query) ')'
    | expr '[' (expr_list | '*') ']'
    | expr ACCESS expr
    | ('+'|'-') expr
    | id_ LRB expr_list RRB
    | expr ('*' | '/' | '%') expr
    | expr ('+' | '-') expr
    | expr ('==' | '!=' | '>' | '<' | '>=' | '<=' | '=~' | '!~') expr
    | expr (ALL | NONE | ANY) (IN | '==' | '>' | '>=' | '<' | '<=' | '!=') expr
    | expr '?' expr ':' expr
    | expr ASSIGN expr
    | expr (',' expr)+
    | expr RANGE expr
    | expr NOT? (IN | LIKE) expr
    | ('!' | NOT)+ expr
    | expr ('&&' | AND) expr
    | expr ('||' | OR) expr
    ;

literal
    : numeric_literal
    | TRUE
    | FALSE
    | string
    | array_literal
    | object_literal
    | NULL_
    | id_
    | BIND_PARAMETER
    | new_old
    ;

array_literal
    : '[' expr_list? ']'
    ;

object_literal
    : '{' (pair_list | id_ (',' id_)*)? '}' // accepted in RETURN clause
    ;

pair_list
    : pair (',' pair)*
    ;

pair
    : (string | id_) ':' expr
    ;

expr_list
    : expr (',' expr)*
    ;

string
    : STRING_LITERAL
    | DOUBLE_QUOTED_STRING_LITERAL
    | BACKSTICK_STRING_LITERAL
    ;

numeric_literal
    : DECIMAL_LITERAL
    | FLOAT_LITERAL
    | REAL_LITERAL
    ;
