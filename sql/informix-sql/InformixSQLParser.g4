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

// root
sqlScript
    : unitStatement* EOF
    ;

unitStatement
    : (createRole
    | closeStmt
    | closeDatabaseStmt
    | commitWorkStmt
    | dropAccessMethod
    | dropAggregate
    | dropDatabase
    | dropIndex
    | dropRole
    | dropSynonym
    | dropTable
    | dropTrigger
    | dropTrustedContext
    | dropType
    | dropUser
    | dropView
    | dropXadatasource
    | dropXadataTypeSource
    | databaseStmt
    | releaseSavepoint
    | renameColumn
    | renameConstraint
    | renameDatabase
    | renameIndex
    | renameSecurity
    | renameSequence
    | renameTable
    | renameTrustedContext
    | renameUser
    | rollbackWork
    | savepointStmt
    | setAutofree
    | setCollation
    | setDataskip
    | setDebugFile
    ) SCOL
    ;

// https://www.ibm.com/docs/en/informix-servers/14.10?topic=statements-create-role-statement
createRole
    : CREATE ROLE (IF NOT EXISTS)? roleName=anyName
    ;

// https://www.ibm.com/docs/en/informix-servers/14.10?topic=statements-drop-role-statement
dropRole
    : DROP ROLE (IF EXISTS)? roleName=anyName
    ;

// https://www.ibm.com/docs/en/informix-servers/14.10?topic=statements-drop-synonym-statement
dropSynonym
    : DROP SYNONYM (IF EXISTS)? synonymName=identifier
    ;

// https://www.ibm.com/docs/en/informix-servers/14.10?topic=statements-drop-table-statement
dropTable
    : DROP TABLE (IF EXISTS)? tableName=identifier (CASCADE | RESTRICT)?
    ;

// https://www.ibm.com/docs/en/informix-servers/14.10?topic=statements-drop-trigger-statement
dropTrigger
    : DROP TRIGGER (IF EXISTS)? triggerName=identifier
    ;

// https://www.ibm.com/docs/en/informix-servers/14.10?topic=statements-drop-trusted-context-statement
dropTrustedContext
    : DROP TRUSTED CONTEXT contextName=anyName
    ;
// https://www.ibm.com/docs/en/informix-servers/14.10?topic=statements-drop-type-statement
dropType
    : DROP TYPE (IF EXISTS)? dataTypeName=identifier RESTRICT
    ;

// https://www.ibm.com/docs/en/informix-servers/14.10?topic=statements-drop-user-statement-unix-linux
dropUser
    : DROP USER userName=anyName
    ;

// https://www.ibm.com/docs/en/informix-servers/14.10?topic=statements-drop-view-statement
dropView
    : DROP VIEW (IF EXISTS)? viewName=identifier (CASCADE | RESTRICT)?
    ;

// https://www.ibm.com/docs/en/informix-servers/14.10?topic=statements-drop-xadatasource-statement
dropXadatasource
    : DROP XADATASOURCE (IF EXISTS)? xaSourceName=identifier RESTRICT
    ;

// https://www.ibm.com/docs/en/informix-servers/14.10?topic=statements-drop-xadatasource-type-statement
dropXadataTypeSource
    : DROP XADATASOURCE TYPE (IF EXISTS)? xaSourceName=identifier RESTRICT
    ;

// https://www.ibm.com/docs/en/informix-servers/14.10?topic=statements-drop-access-method-statement
dropAccessMethod
    : DROP ACCESS_METHOD (IF EXISTS)? accessMethodName=identifier RESTRICT
    ;

// https://www.ibm.com/docs/en/informix-servers/14.10?topic=statements-drop-aggregate-statement
dropAggregate
    : DROP AGGREGATE (IF EXISTS)? aggregateName=identifier
    ;

// https://www.ibm.com/docs/en/informix-servers/14.10?topic=statements-drop-database-statement
dropDatabase
    : DROP DATABASE (IF EXISTS)? databaseName=identifier
    ;

// https://www.ibm.com/docs/en/informix-servers/14.10?topic=statements-drop-index-statement
dropIndex
    : DROP INDEX (IF EXISTS)? indexName=identifier ONLINE?
    ;

// https://www.ibm.com/docs/en/informix-servers/14.10?topic=statements-close-statement
closeStmt
    : CLOSE cursorId=identifier
    ;

// https://www.ibm.com/docs/en/informix-servers/14.10?topic=statements-close-database-statement
closeDatabaseStmt
    : CLOSE DATABASE
    ;

// https://www.ibm.com/docs/en/informix-servers/14.10?topic=statements-database-statement
databaseStmt
    : DATABASE databaseName=anyName EXCLUSIVE?
    ;

// https://www.ibm.com/docs/en/informix-servers/14.10?topic=statements-commit-work-statement
commitWorkStmt
    : COMMIT WORK?
    ;

// https://www.ibm.com/docs/en/informix-servers/14.10?topic=statements-release-savepoint-statement
releaseSavepoint
    : RELEASE SAVEPOINT savepointName=identifier
    ;

// https://www.ibm.com/docs/en/informix-servers/14.10?topic=statements-rename-column-statement
renameColumn
    : RENAME COLUMN oldColumn=identifier TO newColumn=identifier
    ;

// https://www.ibm.com/docs/en/informix-servers/14.10?topic=statements-rename-constraint-statement
renameConstraint
    : RENAME CONSTRAINT oldConstraint=identifier TO newConstraint=identifier
    ;

// https://www.ibm.com/docs/en/informix-servers/14.10?topic=statements-rename-database-statement
renameDatabase
    : RENAME DATABASE oldDatabase=identifier TO newDatabase=identifier
    ;

// https://www.ibm.com/docs/en/informix-servers/14.10?topic=statements-rename-index-statement
renameIndex
    : RENAME INDEX oldIndex=identifier TO newIndex=identifier
    ;

// https://www.ibm.com/docs/en/informix-servers/14.10?topic=statements-rename-security-statement
renameSecurity
    : RENAME SECURITY (POLICY | LABEL (policy=identifier? | COMPONENT)) oldSecurity=identifier TO newSecurity=identifier
    ;

// https://www.ibm.com/docs/en/informix-servers/14.10?topic=statements-rename-sequence-statement
renameSequence
    : RENAME SEQUENCE oldSequence=identifier TO newSequence=identifier
    ;

// https://www.ibm.com/docs/en/informix-servers/14.10?topic=statements-rename-table-statement
renameTable
    : RENAME TABLE oldTableName=identifier TO newTableName=identifier
    ;

// https://www.ibm.com/docs/en/informix-servers/14.10?topic=statements-rename-trusted-context-statement
renameTrustedContext
    : RENAME TRUSTED CONTEXT oldTrustedContextName=identifier TO newTrustedContextName=identifier
    ;

// https://www.ibm.com/docs/en/informix-servers/14.10?topic=statements-rename-user-statement-unix-linux
renameUser
    : RENAME USER oldUserName=identifier TO newUserName=identifier
    ;

// https://www.ibm.com/docs/en/informix-servers/14.10?topic=statements-rollback-work-statement
rollbackWork
    : ROLLBACK WORK? (TO SAVEPOINT savepoint=identifier?)?
    ;

// https://www.ibm.com/docs/en/informix-servers/14.10?topic=statements-savepoint-statement
savepointStmt
    : SAVEPOINT savepoint=identifier UNIQUE?
    ;

// https://www.ibm.com/docs/en/informix-servers/14.10?topic=statements-set-autofree-statement
setAutofree
    : SET AUTOFREE (ENABLED | DISABLED)? (FOR (cursorId=identifier | cursorIdVar=anyName))?
    ;

// https://www.ibm.com/docs/en/informix-servers/14.10?topic=statements-set-collation-statement
setCollation
    : SET (COLLATION locale=quotedString | NO COLLATION)
    ;

// https://www.ibm.com/docs/en/informix-servers/14.10?topic=statements-set-dataskip-statement
setDataskip
    : SET DATASKIP (ON (identifier (COMMA identifier)*)?  | OFF | DEFAULT)
    ;

// https://www.ibm.com/docs/en/informix-servers/14.10?topic=statements-set-debug-file-statement
setDebugFile
    : SET DEBUG FILE TO (CHAR_STRING | identifier) (WITH APPEND)?
    ;

quotedString
    : CHAR_STRING
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
    | ACCESS_METHOD
    | ADD
    | AFTER
    | AGGREGATE
    | ALL
    | ALTER
    | ANALYZE
    | AND
    | APPEND
    | AS
    | ASC
    | ATTACH
    | AUTOFREE
    | AUTOINCREMENT
    | BEFORE
    | BEGIN
    | BETWEEN
    | BY
    | CASCADE
    | CASE
    | CAST
    | CHECK
    | CLOSE
    | COLLATE
    | COLLATION
    | COLUMN
    | COMPONENT
    | COMMIT
    | CONFLICT
    | CONSTRAINT
    | CONTEXT
    | CREATE
    | CROSS
    | CURRENT_DATE
    | CURRENT_TIME
    | CURRENT_TIMESTAMP
    | DATABASE
    | DATASKIP
    | DEBUG
    | DEFAULT
    | DEFERRABLE
    | DEFERRED
    | DELETE
    | DESC
    | DETACH
    | DISABLED
    | DISTINCT
    | DROP
    | EACH
    | ELSE
    | ENABLED
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
    | FILE
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
    | LABEL
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
    | OFF
    | OFFSET
    | ON
    | ONLINE
    | OR
    | ORDER
    | OUTER
    | POLICY
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
    | SECURITY
    | SELECT
    | SET
    | SEQUENCE
    | SYNONYM
    | TABLE
    | TEMP
    | TEMPORARY
    | THEN
    | TO
    | TRANSACTION
    | TRIGGER
    | TRUSTED
    | TYPE
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
    | WORK
    | XADATASOURCE
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