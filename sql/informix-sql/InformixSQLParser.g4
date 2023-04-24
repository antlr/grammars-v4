/*
 * The MIT License (MIT)
 *
 * Copyright (c) 2023 by 455741807@qq.com
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy of this software and
 * associated documentation files (the "Software"), to deal in the Software without restriction,
 * including without limitation the rights to use, copy, modify, merge, publish, distribute,
 * sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in all copies or
 * substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT
 * NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
 * NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM,
 * DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
 *
 * Project : InformixSQLParser; an ANTLR4 grammar for Informix database SQL language,
 * see as https://www.ibm.com/docs/en/informix-servers/14.10?topic=programming-guide-sql-syntax
 */

parser grammar InformixSQLParser;

options {
    tokenVocab = InformixSQLLexer;
}

sqlScript
    : unitStatement* EOF
    ;

unitStatement
    : (createRole
    | dropRole
    | dropTable
    | dropUser
    ) SCOL
    ;

//https://www.ibm.com/docs/en/informix-servers/14.10?topic=statements-create-role-statement
createRole
    : CREATE ROLE (IF NOT EXISTS)? roleName
    ;

//https://www.ibm.com/docs/en/informix-servers/14.10?topic=statements-drop-role-statement
dropRole
    : DROP ROLE (IF EXISTS)? roleName
    ;

//https://www.ibm.com/docs/en/informix-servers/14.10?topic=statements-drop-table-statement
dropTable
    : DROP TABLE (IF EXISTS)? tableName (CASCADE | RESTRICT)?
    ;

//https://www.ibm.com/docs/en/informix-servers/14.10?topic=statements-drop-user-statement-unix-linux
dropUser
    : DROP USER userName
    ;

roleName
    : anyName
    ;

tableName
    : identifier
    ;

userName
    : anyName
    ;

anyName
    : IDENTIFIER
    | keyword
    | STRING_LITERAL
    | OPEN_PAR anyName CLOSE_PAR
;

identifier
    : anyName ('.' anyName)*
    ;

keyword
    : ABORT
    | ACTION
    | ADD
    | AFTER
    | ALL
    | ALTER
    | ANALYZE
    | AND
    | AS
    | ASC
    | ATTACH
    | AUTOINCREMENT
    | BEFORE
    | BEGIN
    | BETWEEN
    | BY
    | CASCADE
    | CASE
    | CAST
    | CHECK
    | COLLATE
    | COLUMN
    | COMMIT
    | CONFLICT
    | CONSTRAINT
    | CREATE
    | CROSS
    | CURRENT_DATE
    | CURRENT_TIME
    | CURRENT_TIMESTAMP
    | DATABASE
    | DEFAULT
    | DEFERRABLE
    | DEFERRED
    | DELETE
    | DESC
    | DETACH
    | DISTINCT
    | DROP
    | EACH
    | ELSE
    | END
    | ESCAPE
    | EXCEPT
    | EXCLUSIVE
    | EXISTS
    | EXPLAIN
    | FAIL
    | FOR
    | FOREIGN
    | FROM
    | FULL
    | GLOB
    | GROUP
    | HAVING
    | IF
    | IGNORE
    | IMMEDIATE
    | IN
    | INDEX
    | INDEXED
    | INITIALLY
    | INNER
    | INSERT
    | INSTEAD
    | INTERSECT
    | INTO
    | IS
    | ISNULL
    | JOIN
    | KEY
    | LEFT
    | LIKE
    | LIMIT
    | MATCH
    | NATURAL
    | NO
    | NOT
    | NOTNULL
    | NULL
    | OF
    | OFFSET
    | ON
    | OR
    | ORDER
    | OUTER
    | PLAN
    | PRAGMA
    | PRIMARY
    | QUERY
    | RAISE
    | RECURSIVE
    | REFERENCES
    | REGEXP
    | REINDEX
    | RELEASE
    | RENAME
    | REPLACE
    | RESTRICT
    | RIGHT
    | ROLLBACK
    | ROW
    | ROWS
    | SAVEPOINT
    | SELECT
    | SET
    | TABLE
    | TEMP
    | TEMPORARY
    | THEN
    | TO
    | TRANSACTION
    | TRIGGER
    | UNION
    | UNIQUE
    | UPDATE
    | USER
    | USING
    | VACUUM
    | VALUES
    | VIEW
    | VIRTUAL
    | WHEN
    | WHERE
    | WITH
    | WITHOUT
    | FIRST_VALUE
    | OVER
    | PARTITION
    | RANGE
    | PRECEDING
    | UNBOUNDED
    | CURRENT
    | FOLLOWING
    | CUME_DIST
    | DENSE_RANK
    | LAG
    | LAST_VALUE
    | LEAD
    | NTH_VALUE
    | NTILE
    | PERCENT_RANK
    | RANK
    | ROW_NUMBER
    | GENERATED
    | ALWAYS
    | STORED
    | TRUE
    | FALSE
    | WINDOW
    | NULLS
    | FIRST
    | LAST
    | FILTER
    | GROUPS
    | EXCLUDE
;