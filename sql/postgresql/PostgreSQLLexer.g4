/*
based on
https://github.com/tunnelvisionlabs/antlr4-grammar-postgresql/blob/master/src/com/tunnelvisionlabs/postgresql/PostgreSqlLexer.g4
*/

/*
 * [The "MIT license"]
 * Copyright (C) 2014 Sam Harwell, Tunnel Vision Laboratories, LLC
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * 1. The above copyright notice and this permission notice shall be included in
 *    all copies or substantial portions of the Software.
 * 2. THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 *    IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 *    FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL
 *    THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 *    LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
 *    FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
 *    DEALINGS IN THE SOFTWARE.
 * 3. Except as contained in this notice, the name of Tunnel Vision
 *    Laboratories, LLC. shall not be used in advertising or otherwise to
 *    promote the sale, use or other dealings in this Software without prior
 *    written authorization from Tunnel Vision Laboratories, LLC.
 */
lexer grammar PostgreSQLLexer;
/* Reference:
 * http://www.postgresql.org/docs/9.3/static/sql-syntax-lexical.html
 */



options {
superClass = PostgreSQLLexerBase;
}

@ header
{
}
@ members
{
/* This field stores the tags which are used to detect the end of a dollar-quoted string literal.
 */
}
//

// SPECIAL CHARACTERS (4.1.4)

//

// Note that Asterisk is a valid operator, but does not have the type Operator due to its syntactic use in locations

// that are not expressions.

Dollar
   : '$'
   ;

OPEN_PAREN
   : '('
   ;

CLOSE_PAREN
   : ')'
   ;

OPEN_BRACKET
   : '['
   ;

CLOSE_BRACKET
   : ']'
   ;

COMMA
   : ','
   ;

SEMI
   : ';'
   ;

COLON
   : ':'
   ;

STAR
   : '*'
   ;

EQUAL
   : '='
   ;

DOT
   : '.'
   ;
   //NamedArgument	: ':=';

PLUS
   : '+'
   ;

MINUS
   : '-'
   ;

SLASH
   : '/'
   ;

CARET
   : '^'
   ;

LT
   : '<'
   ;

GT
   : '>'
   ;

LESS_LESS
   : '<<'
   ;

GREATER_GREATER
   : '>>'
   ;

COLON_EQUALS
   : ':='
   ;

LESS_EQUALS
   : '<='
   ;

EQUALS_GREATER
   : '=>'
   ;

GREATER_EQUALS
   : '>='
   ;

DOT_DOT
   : '..'
   ;

NOT_EQUALS
   : '<>'
   ;

TYPECAST
   : '::'
   ;

PERCENT
   : '%'
   ;

PARAM
   : '$' ([0-9])+
   ;
   //

   // OPERATORS (4.1.3)

   //

   // this rule does not allow + or - at the end of a multi-character operator

Operator
   : ((OperatorCharacter | ('+' | '-'
   {checkLA('-')}?)+ (OperatorCharacter | '/'
   {checkLA('*')}?) | '/'
   {checkLA('*')}?)+ | // special handling for the single-character operators + and -
   [+-])
   //TODO somehow rewrite this part without using Actions

   {
    HandleLessLessGreaterGreater();
   }
   ;
/* This rule handles operators which end with + or -, and sets the token type to Operator. It is comprised of four
 * parts, in order:
 *
 *   1. A prefix, which does not contain a character from the required set which allows + or - to appear at the end of
 *      the operator.
 *   2. A character from the required set which allows + or - to appear at the end of the operator.
 *   3. An optional sub-token which takes the form of an operator which does not include a + or - at the end of the
 *      sub-token.
 *   4. A suffix sequence of + and - characters.
 */


OperatorEndingWithPlusMinus
   : (OperatorCharacterNotAllowPlusMinusAtEnd | '-'
   {checkLA('-')}? | '/'
   {checkLA('*')}?)* OperatorCharacterAllowPlusMinusAtEnd Operator? ('+' | '-'
   {checkLA('-')}?)+ -> type (Operator)
   ;
   // Each of the following fragment rules omits the +, -, and / characters, which must always be handled in a special way

   // by the operator rules above.

fragment OperatorCharacter
   : [*<>=~!@%^&|`?#]
   ;
   // these are the operator characters that don't count towards one ending with + or -

fragment OperatorCharacterNotAllowPlusMinusAtEnd
   : [*<>=+]
   ;
   // an operator may end with + or - if it contains one of these characters

fragment OperatorCharacterAllowPlusMinusAtEnd
   : [~!@%^&|`?#]
   ;
   //

   // KEYWORDS (Appendix C)

   //

fragment A
   : [aA]
   ;

fragment B
   : [bB]
   ;

fragment C
   : [cC]
   ;

fragment D
   : [dD]
   ;

fragment E
   : [eE]
   ;

fragment F
   : [fF]
   ;

fragment G
   : [gG]
   ;

fragment H
   : [hH]
   ;

fragment I
   : [iI]
   ;

fragment J
   : [jJ]
   ;

fragment K
   : [kK]
   ;

fragment L
   : [lL]
   ;

fragment M
   : [mM]
   ;

fragment N
   : [nN]
   ;

fragment O
   : [oO]
   ;

fragment P
   : [pP]
   ;

fragment Q
   : [qQ]
   ;

fragment R
   : [rR]
   ;

fragment S
   : [sS]
   ;

fragment T
   : [tT]
   ;

fragment U
   : [uU]
   ;

fragment V
   : [vV]
   ;

fragment W
   : [wW]
   ;

fragment X
   : [xX]
   ;

fragment Y
   : [yY]
   ;

fragment Z
   : [zZ]
   ;
   //

   // reserved keywords

   //

ALL
   : A L L
   ;

ANALYSE
   : A N A L Y S E
   ;

ANALYZE
   : A N A L Y Z E
   ;

AND
   : A N D
   ;

ANY
   : A N Y
   ;

ARRAY
   : A R R A Y
   ;

AS
   : A S
   ;

ASC
   : A S C
   ;

ASYMMETRIC
   : A S Y M M E T R I C
   ;

BOTH
   : B O T H
   ;

CASE
   : C A S E
   ;

CAST
   : C A S T
   ;

CHECK
   : C H E C K
   ;

COLLATE
   : C O L L A T E
   ;

COLUMN
   : C O L U M N
   ;

CONSTRAINT
   : C O N S T R A I N T
   ;

CREATE
   : C R E A T E
   ;

CURRENT_CATALOG
   : C U R R E N T '_' C A T A L O G
   ;

CURRENT_DATE
   : C U R R E N T '_' D A T E
   ;

CURRENT_ROLE
   : C U R R E N T '_' R O L E
   ;

CURRENT_TIME
   : C U R R E N T '_' T I M E
   ;

CURRENT_TIMESTAMP
   : C U R R E N T '_' T I M E S T A M P
   ;

CURRENT_USER
   : C U R R E N T '_' U S E R
   ;

DEFAULT
   : D E F A U L T
   ;

DEFERRABLE
   : D E F E R R A B L E
   ;

DESC
   : D E S C
   ;

DISTINCT
   : D I S T I N C T
   ;

DO
   : D O
   ;

ELSE
   : E L S E
   ;

EXCEPT
   : E X C E P T
   ;

FALSE_P
   : F A L S E
   ;

FETCH
   : F E T C H
   ;

FOR
   : F O R
   ;

FOREIGN
   : F O R E I G N
   ;

FROM
   : F R O M
   ;

GRANT
   : G R A N T
   ;

GROUP_P
   : G R O U P
   ;

HAVING
   : H A V I N G
   ;

IN_P
   : I N
   ;

INITIALLY
   : I N I T I A L L Y
   ;

INTERSECT
   : I N T E R S E C T
   ;

INTO
   : I N T O
   ;

LATERAL_P
   : L A T E R A L
   ;

LEADING
   : L E A D I N G
   ;

LIMIT
   : L I M I T
   ;

LOCALTIME
   : L O C A L T I M E
   ;

LOCALTIMESTAMP
   : L O C A L T I M E S T A M P
   ;

NOT
   : N O T
   ;

NULL_P
   : N U L L
   ;

OFFSET
   : O F F S E T
   ;

ON
   : O N
   ;

ONLY
   : O N L Y
   ;

OR
   : O R
   ;

ORDER
   : O R D E R
   ;

PLACING
   : P L A C I N G
   ;

PRIMARY
   : P R I M A R Y
   ;

REFERENCES
   : R E F E R E N C E S
   ;

RETURNING
   : R E T U R N I N G
   ;

SELECT
   : S E L E C T
   ;

SESSION_USER
   : S E S S I O N '_' U S E R
   ;

SOME
   : S O M E
   ;

SYMMETRIC
   : S Y M M E T R I C
   ;

TABLE
   : T A B L E
   ;

THEN
   : T H E N
   ;

TO
   : T O
   ;

TRAILING
   : T R A I L I N G
   ;

TRUE_P
   : T R U E
   ;

UNION
   : U N I O N
   ;

UNIQUE
   : U N I Q U E
   ;

USER
   : U S E R
   ;

USING
   : U S I N G
   ;

VARIADIC
   : V A R I A D I C
   ;

WHEN
   : W H E N
   ;

WHERE
   : W H E R E
   ;

WINDOW
   : W I N D O W
   ;

WITH
   : W I T H
   ;
   //

   // reserved keywords (can be function or type)

   //

AUTHORIZATION
   : A U T H O R I Z A T I O N
   ;

BINARY
   : B I N A R Y
   ;

COLLATION
   : C O L L A T I O N
   ;

CONCURRENTLY
   : C O N C U R R E N T L Y
   ;

CROSS
   : C R O S S
   ;

CURRENT_SCHEMA
   : C U R R E N T '_' S C H E M A
   ;

FREEZE
   : F R E E Z E
   ;

FULL
   : F U L L
   ;

ILIKE
   : I L I K E
   ;

INNER_P
   : I N N E R
   ;

IS
   : I S
   ;

ISNULL
   : I S N U L L
   ;

JOIN
   : J O I N
   ;

LEFT
   : L E F T
   ;

LIKE
   : L I K E
   ;

NATURAL
   : N A T U R A L
   ;

NOTNULL
   : N O T N U L L
   ;

OUTER_P
   : O U T E R
   ;

OVER
   : O V E R
   ;

OVERLAPS
   : O V E R L A P S
   ;

RIGHT
   : R I G H T
   ;

SIMILAR
   : S I M I L A R
   ;

VERBOSE
   : V E R B O S E
   ;
   //

   // non-reserved keywords

   //

ABORT_P
   : A B O R T
   ;

ABSOLUTE_P
   : A B S O L U T E
   ;

ACCESS
   : A C C E S S
   ;

ACTION
   : A C T I O N
   ;

ADD_P
   : A D D
   ;

ADMIN
   : A D M I N
   ;

AFTER
   : A F T E R
   ;

AGGREGATE
   : A G G R E G A T E
   ;

ALSO
   : A L S O
   ;

ALTER
   : A L T E R
   ;

ALWAYS
   : A L W A Y S
   ;

ASSERTION
   : A S S E R T I O N
   ;

ASSIGNMENT
   : A S S I G N M E N T
   ;

AT
   : A T
   ;

ATTRIBUTE
   : A T T R I B U T E
   ;

BACKWARD
   : B A C K W A R D
   ;

BEFORE
   : B E F O R E
   ;

BEGIN_P
   : B E G I N
   ;

BY
   : B Y
   ;

CACHE
   : C A C H E
   ;

CALLED
   : C A L L E D
   ;

CASCADE
   : C A S C A D E
   ;

CASCADED
   : C A S C A D E D
   ;

CATALOG
   : C A T A L O G
   ;

CHAIN
   : C H A I N
   ;

CHARACTERISTICS
   : C H A R A C T E R I S T I C S
   ;

CHECKPOINT
   : C H E C K P O I N T
   ;

CLASS
   : C L A S S
   ;

CLOSE
   : C L O S E
   ;

CLUSTER
   : C L U S T E R
   ;

COMMENT
   : C O M M E N T
   ;

COMMENTS
   : C O M M E N T S
   ;

COMMIT
   : C O M M I T
   ;

COMMITTED
   : C O M M I T T E D
   ;

CONFIGURATION
   : C O N F I G U R A T I O N
   ;

CONNECTION
   : C O N N E C T I O N
   ;

CONSTRAINTS
   : C O N S T R A I N T S
   ;

CONTENT_P
   : C O N T E N T
   ;

CONTINUE_P
   : C O N T I N U E
   ;

CONVERSION_P
   : C O N V E R S I O N
   ;

COPY
   : C O P Y
   ;

COST
   : C O S T
   ;

CSV
   : C S V
   ;

CURSOR
   : C U R S O R
   ;

CYCLE
   : C Y C L E
   ;

DATA_P
   : D A T A
   ;

DATABASE
   : D A T A B A S E
   ;

DAY_P
   : D A Y
   ;

DEALLOCATE
   : D E A L L O C A T E
   ;

DECLARE
   : D E C L A R E
   ;

DEFAULTS
   : D E F A U L T S
   ;

DEFERRED
   : D E F E R R E D
   ;

DEFINER
   : D E F I N E R
   ;

DELETE_P
   : D E L E T E
   ;

DELIMITER
   : D E L I M I T E R
   ;

DELIMITERS
   : D E L I M I T E R S
   ;

DICTIONARY
   : D I C T I O N A R Y
   ;

DISABLE_P
   : D I S A B L E
   ;

DISCARD
   : D I S C A R D
   ;

DOCUMENT_P
   : D O C U M E N T
   ;

DOMAIN_P
   : D O M A I N
   ;

DOUBLE_P
   : D O U B L E
   ;

DROP
   : D R O P
   ;

EACH
   : E A C H
   ;

ENABLE_P
   : E N A B L E
   ;

ENCODING
   : E N C O D I N G
   ;

ENCRYPTED
   : E N C R Y P T E D
   ;

ENUM_P
   : E N U M
   ;

ESCAPE
   : E S C A P E
   ;

EVENT
   : E V E N T
   ;

EXCLUDE
   : E X C L U D E
   ;

EXCLUDING
   : E X C L U D I N G
   ;

EXCLUSIVE
   : E X C L U S I V E
   ;

EXECUTE
   : E X E C U T E
   ;

EXPLAIN
   : E X P L A I N
   ;

EXTENSION
   : E X T E N S I O N
   ;

EXTERNAL
   : E X T E R N A L
   ;

FAMILY
   : F A M I L Y
   ;

FIRST_P
   : F I R S T
   ;

FOLLOWING
   : F O L L O W I N G
   ;

FORCE
   : F O R C E
   ;

FORWARD
   : F O R W A R D
   ;

FUNCTION
   : F U N C T I O N
   ;

FUNCTIONS
   : F U N C T I O N S
   ;

GLOBAL
   : G L O B A L
   ;

GRANTED
   : G R A N T E D
   ;

HANDLER
   : H A N D L E R
   ;

HEADER_P
   : H E A D E R
   ;

HOLD
   : H O L D
   ;

HOUR_P
   : H O U R
   ;

IDENTITY_P
   : I D E N T I T Y
   ;

IF_P
   : I F
   ;

IMMEDIATE
   : I M M E D I A T E
   ;

IMMUTABLE
   : I M M U T A B L E
   ;

IMPLICIT_P
   : I M P L I C I T
   ;

INCLUDING
   : I N C L U D I N G
   ;

INCREMENT
   : I N C R E M E N T
   ;

INDEX
   : I N D E X
   ;

INDEXES
   : I N D E X E S
   ;

INHERIT
   : I N H E R I T
   ;

INHERITS
   : I N H E R I T S
   ;

INLINE_P
   : I N L I N E
   ;

INSENSITIVE
   : I N S E N S I T I V E
   ;

INSERT
   : I N S E R T
   ;

INSTEAD
   : I N S T E A D
   ;

INVOKER
   : I N V O K E R
   ;

ISOLATION
   : I S O L A T I O N
   ;

KEY
   : K E Y
   ;

LABEL
   : L A B E L
   ;

LANGUAGE
   : L A N G U A G E
   ;

LARGE_P
   : L A R G E
   ;

LAST_P
   : L A S T
   ;
   //LC_COLLATE			: L C '_'C O L L A T E ;

   //LC_CTYPE			: L C '_'C T Y P E ;

LEAKPROOF
   : L E A K P R O O F
   ;

LEVEL
   : L E V E L
   ;

LISTEN
   : L I S T E N
   ;

LOAD
   : L O A D
   ;

LOCAL
   : L O C A L
   ;

LOCATION
   : L O C A T I O N
   ;

LOCK_P
   : L O C K
   ;

MAPPING
   : M A P P I N G
   ;

MATCH
   : M A T C H
   ;

MATERIALIZED
   : M A T E R I A L I Z E D
   ;

MAXVALUE
   : M A X V A L U E
   ;

MINUTE_P
   : M I N U T E
   ;

MINVALUE
   : M I N V A L U E
   ;

MODE
   : M O D E
   ;

MONTH_P
   : M O N T H
   ;

MOVE
   : M O V E
   ;

NAME_P
   : N A M E
   ;

NAMES
   : N A M E S
   ;

NEXT
   : N E X T
   ;

NO
   : N O
   ;

NOTHING
   : N O T H I N G
   ;

NOTIFY
   : N O T I F Y
   ;

NOWAIT
   : N O W A I T
   ;

NULLS_P
   : N U L L S
   ;

OBJECT_P
   : O B J E C T
   ;

OF
   : O F
   ;

OFF
   : O F F
   ;

OIDS
   : O I D S
   ;

OPERATOR
   : O P E R A T O R
   ;

OPTION
   : O P T I O N
   ;

OPTIONS
   : O P T I O N S
   ;

OWNED
   : O W N E D
   ;

OWNER
   : O W N E R
   ;

PARSER
   : P A R S E R
   ;

PARTIAL
   : P A R T I A L
   ;

PARTITION
   : P A R T I T I O N
   ;

PASSING
   : P A S S I N G
   ;

PASSWORD
   : P A S S W O R D
   ;

PLANS
   : P L A N S
   ;

PRECEDING
   : P R E C E D I N G
   ;

PREPARE
   : P R E P A R E
   ;

PREPARED
   : P R E P A R E D
   ;

PRESERVE
   : P R E S E R V E
   ;

PRIOR
   : P R I O R
   ;

PRIVILEGES
   : P R I V I L E G E S
   ;

PROCEDURAL
   : P R O C E D U R A L
   ;

PROCEDURE
   : P R O C E D U R E
   ;

PROGRAM
   : P R O G R A M
   ;

QUOTE
   : Q U O T E
   ;

RANGE
   : R A N G E
   ;

READ
   : R E A D
   ;

REASSIGN
   : R E A S S I G N
   ;

RECHECK
   : R E C H E C K
   ;

RECURSIVE
   : R E C U R S I V E
   ;

REF
   : R E F
   ;

REFRESH
   : R E F R E S H
   ;

REINDEX
   : R E I N D E X
   ;

RELATIVE_P
   : R E L A T I V E
   ;

RELEASE
   : R E L E A S E
   ;

RENAME
   : R E N A M E
   ;

REPEATABLE
   : R E P E A T A B L E
   ;

REPLACE
   : R E P L A C E
   ;

REPLICA
   : R E P L I C A
   ;

RESET
   : R E S E T
   ;

RESTART
   : R E S T A R T
   ;

RESTRICT
   : R E S T R I C T
   ;

RETURNS
   : R E T U R N S
   ;

REVOKE
   : R E V O K E
   ;

ROLE
   : R O L E
   ;

ROLLBACK
   : R O L L B A C K
   ;

ROWS
   : R O W S
   ;

RULE
   : R U L E
   ;

SAVEPOINT
   : S A V E P O I N T
   ;

SCHEMA
   : S C H E M A
   ;

SCROLL
   : S C R O L L
   ;

SEARCH
   : S E A R C H
   ;

SECOND_P
   : S E C O N D
   ;

SECURITY
   : S E C U R I T Y
   ;

SEQUENCE
   : S E Q U E N C E
   ;

SEQUENCES
   : S E Q U E N C E S
   ;

SERIALIZABLE
   : S E R I A L I Z A B L E
   ;

SERVER
   : S E R V E R
   ;

SESSION
   : S E S S I O N
   ;

SET
   : S E T
   ;

SHARE
   : S H A R E
   ;

SHOW
   : S H O W
   ;

SIMPLE
   : S I M P L E
   ;

SNAPSHOT
   : S N A P S H O T
   ;

STABLE
   : S T A B L E
   ;

STANDALONE_P
   : S T A N D A L O N E
   ;

START
   : S T A R T
   ;

STATEMENT
   : S T A T E M E N T
   ;

STATISTICS
   : S T A T I S T I C S
   ;

STDIN
   : S T D I N
   ;

STDOUT
   : S T D O U T
   ;

STORAGE
   : S T O R A G E
   ;

STRICT_P
   : S T R I C T
   ;

STRIP_P
   : S T R I P
   ;

SYSID
   : S Y S I D
   ;

SYSTEM_P
   : S Y S T E M
   ;

TABLES
   : T A B L E S
   ;

TABLESPACE
   : T A B L E S P A C E
   ;

TEMP
   : T E M P
   ;

TEMPLATE
   : T E M P L A T E
   ;

TEMPORARY
   : T E M P O R A R Y
   ;

TEXT_P
   : T E X T
   ;

TRANSACTION
   : T R A N S A C T I O N
   ;

TRIGGER
   : T R I G G E R
   ;

TRUNCATE
   : T R U N C A T E
   ;

TRUSTED
   : T R U S T E D
   ;

TYPE_P
   : T Y P E
   ;

TYPES_P
   : T Y P E S
   ;

UNBOUNDED
   : U N B O U N D E D
   ;

UNCOMMITTED
   : U N C O M M I T T E D
   ;

UNENCRYPTED
   : U N E N C R Y P T E D
   ;

UNKNOWN
   : U N K N O W N
   ;

UNLISTEN
   : U N L I S T E N
   ;

UNLOGGED
   : U N L O G G E D
   ;

UNTIL
   : U N T I L
   ;

UPDATE
   : U P D A T E
   ;

VACUUM
   : V A C U U M
   ;

VALID
   : V A L I D
   ;

VALIDATE
   : V A L I D A T E
   ;

VALIDATOR
   : V A L I D A T O R
   ;
   //VALUE				: V A L U E ;

VARYING
   : V A R Y I N G
   ;

VERSION_P
   : V E R S I O N
   ;

VIEW
   : V I E W
   ;

VOLATILE
   : V O L A T I L E
   ;

WHITESPACE_P
   : W H I T E S P A C E
   ;

WITHOUT
   : W I T H O U T
   ;

WORK
   : W O R K
   ;

WRAPPER
   : W R A P P E R
   ;

WRITE
   : W R I T E
   ;

XML_P
   : X M L
   ;

YEAR_P
   : Y E A R
   ;

YES_P
   : Y E S
   ;

ZONE
   : Z O N E
   ;
   //

   // non-reserved keywords (cannot be function or type)

   //

BETWEEN
   : B E T W E E N
   ;

BIGINT
   : B I G I N T
   ;

BIT
   : B I T
   ;

BOOLEAN_P
   : B O O L E A N
   ;

CHAR_P
   : C H A R
   ;

CHARACTER
   : C H A R A C T E R
   ;

COALESCE
   : C O A L E S C E
   ;

DEC
   : D E C
   ;

DECIMAL_P
   : D E C I M A L
   ;

EXISTS
   : E X I S T S
   ;

EXTRACT
   : E X T R A C T
   ;

FLOAT_P
   : F L O A T
   ;

GREATEST
   : G R E A T E S T
   ;

INOUT
   : I N O U T
   ;

INT_P
   : I N T
   ;

INTEGER
   : I N T E G E R
   ;

INTERVAL
   : I N T E R V A L
   ;

LEAST
   : L E A S T
   ;

NATIONAL
   : N A T I O N A L
   ;

NCHAR
   : N C H A R
   ;

NONE
   : N O N E
   ;

NULLIF
   : N U L L I F
   ;

NUMERIC
   : N U M E R I C
   ;

OVERLAY
   : O V E R L A Y
   ;

POSITION
   : P O S I T I O N
   ;

PRECISION
   : P R E C I S I O N
   ;

REAL
   : R E A L
   ;

ROW
   : R O W
   ;

SETOF
   : S E T O F
   ;

SMALLINT
   : S M A L L I N T
   ;

SUBSTRING
   : S U B S T R I N G
   ;

TIME
   : T I M E
   ;

TIMESTAMP
   : T I M E S T A M P
   ;

TREAT
   : T R E A T
   ;

TRIM
   : T R I M
   ;

VALUES
   : V A L U E S
   ;

VARCHAR
   : V A R C H A R
   ;

XMLATTRIBUTES
   : X M L A T T R I B U T E S
   ;

XMLCONCAT
   : X M L C O N C A T
   ;

XMLELEMENT
   : X M L E L E M E N T
   ;

XMLEXISTS
   : X M L E X I S T S
   ;

XMLFOREST
   : X M L F O R E S T
   ;

XMLPARSE
   : X M L P A R S E
   ;

XMLPI
   : X M L P I
   ;

XMLROOT
   : X M L R O O T
   ;

XMLSERIALIZE
   : X M L S E R I A L I Z E
   ;
   //MISSED

CALL
   : C A L L
   ;

CURRENT_P
   : C U R R E N T
   ;

CATALOG_P
   : C A T A L O G
   ;

ATTACH
   : A T T A C H
   ;

DETACH
   : D E T A C H
   ;

EXPRESSION
   : E X P R E S S I O N
   ;

GENERATED
   : G E N E R A T E D
   ;

LOGGED
   : L O G G E D
   ;

STORED
   : S T O R E D
   ;

INCLUDE
   : I N C L U D E
   ;

ROUTINE
   : R O U T I N E
   ;

TRANSFORM
   : T R A N S F O R M
   ;

IMPORT_P
   : I M P O R T
   ;

POLICY
   : P O L I C Y
   ;

METHOD
   : M E T H O D
   ;

REFERENCING
   : R E F E R E N C I N G
   ;

NEW
   : N E W
   ;

OLD
   : O L D
   ;

VALUE_P
   : V A L U E
   ;

SUBSCRIPTION
   : S U B S C R I P T I O N
   ;

PUBLICATION
   : P U B L I C A T I O N
   ;

OUT_P
   : O U T
   ;

END_P
   : E N D
   ;

ROUTINES
   : R O U T I N E S
   ;

SCHEMAS
   : S C H E M A S
   ;

PROCEDURES
   : P R O C E D U R E S
   ;

INPUT_P
   : I N P U T
   ;

SUPPORT
   : S U P P O R T
   ;

PARALLEL
   : P A R A L L E L
   ;

SQL_P
   : S Q L
   ;

DEPENDS
   : D E P E N D S
   ;

OVERRIDING
   : O V E R R I D I N G
   ;

CONFLICT
   : C O N F L I C T
   ;

SKIP_P
   : S K I P
   ;

LOCKED
   : L O C K E D
   ;

TIES
   : T I E S
   ;

ROLLUP
   : R O L L U P
   ;

CUBE
   : C U B E
   ;

GROUPING
   : G R O U P I N G
   ;

SETS
   : S E T S
   ;

TABLESAMPLE
   : T A B L E S A M P L E
   ;

ORDINALITY
   : O R D I N A L I T Y
   ;

XMLTABLE
   : X M L T A B L E
   ;

COLUMNS
   : C O L U M N S
   ;

XMLNAMESPACES
   : X M L N A M E S P A C E S
   ;

ROWTYPE
   : R O W T Y P E
   ;

NORMALIZED
   : N O R M A L I Z E D
   ;

WITHIN
   : W I T H I N
   ;

FILTER
   : F I L T E R
   ;

GROUPS
   : G R O U P S
   ;

OTHERS
   : O T H E R S
   ;

NFC
   : N F C
   ;

NFD
   : N F D
   ;

NFKC
   : N F K C
   ;

NFKD
   : N F K D
   ;

UESCAPE
   : U E S C A P E
   ;

VIEWS
   : V I E W S
   ;

NORMALIZE
   : N O R M A L I Z E
   ;

DUMP
   : D U M P
   ;

PRINT_STRICT_PARAMS
   : P R I N T '_' S T R I C T '_' P A R A M S
   ;

VARIABLE_CONFLICT
   : V A R I A B L E '_' C O N F L I C T
   ;

ERROR
   : E R R O R
   ;

USE_VARIABLE
   : U S E '_' V A R I A B L E
   ;

USE_COLUMN
   : U S E '_' C O L U M N
   ;

ALIAS
   : A L I A S
   ;

CONSTANT
   : C O N S T A N T
   ;

PERFORM
   : P E R F O R M
   ;

GET
   : G E T
   ;

DIAGNOSTICS
   : D I A G N O S T I C S
   ;

STACKED
   : S T A C K E D
   ;

ELSIF
   : E L S I F
   ;

WHILE
   : W H I L E
   ;

REVERSE
   : R E V E R S E
   ;

FOREACH
   : F O R E A C H
   ;

SLICE
   : S L I C E
   ;

EXIT
   : E X I T
   ;

RETURN
   : R E T U R N
   ;

QUERY
   : Q U E R Y
   ;

RAISE
   : R A I S E
   ;

SQLSTATE
   : S Q L S T A T E
   ;

DEBUG
   : D E B U G
   ;

LOG
   : L O G
   ;

INFO
   : I N F O
   ;

NOTICE
   : N O T I C E
   ;

WARNING
   : W A R N I N G
   ;

EXCEPTION
   : E X C E P T I O N
   ;

ASSERT
   : A S S E R T
   ;

LOOP
   : L O O P
   ;

OPEN
   : O P E N
   ;
   //

   // IDENTIFIERS (4.1.1)

   //

Identifier
   : IdentifierStartChar IdentifierChar*
   ;

fragment IdentifierStartChar
   : // these are the valid identifier start characters below 0x7F
   [a-zA-Z_]
   | // these are the valid characters from 0x80 to 0xFF
   [\u00AA\u00B5\u00BA\u00C0-\u00D6\u00D8-\u00F6\u00F8-\u00FF]
   | // these are the letters above 0xFF which only need a single UTF-16 code unit
   [\u0100-\uD7FF\uE000-\uFFFF]
   {charIsLetter()}?
   | // letters which require multiple UTF-16 code units
   [\uD800-\uDBFF] [\uDC00-\uDFFF]
   {
    CheckIfUtf32Letter()
   }?

   ;

fragment IdentifierChar
   : StrictIdentifierChar
   | '$'
   ;

fragment StrictIdentifierChar
   : IdentifierStartChar
   | [0-9]
   ;
/* Quoted Identifiers
 *
 *   These are divided into four separate tokens, allowing distinction of valid quoted identifiers from invalid quoted
 *   identifiers without sacrificing the ability of the lexer to reliably recover from lexical errors in the input.
 */


QuotedIdentifier
   : UnterminatedQuotedIdentifier '"'
   ;
   // This is a quoted identifier which only contains valid characters but is not terminated

UnterminatedQuotedIdentifier
   : '"' ('""' | ~ [\u0000"])*
   ;
   // This is a quoted identifier which is terminated but contains a \u0000 character

InvalidQuotedIdentifier
   : InvalidUnterminatedQuotedIdentifier '"'
   ;
   // This is a quoted identifier which is unterminated and contains a \u0000 character

InvalidUnterminatedQuotedIdentifier
   : '"' ('""' | ~ '"')*
   ;
/* Unicode Quoted Identifiers
 *
 *   These are divided into four separate tokens, allowing distinction of valid Unicode quoted identifiers from invalid
 *   Unicode quoted identifiers without sacrificing the ability of the lexer to reliably recover from lexical errors in
 *   the input. Note that escape sequences are never checked as part of this determination due to the ability of users
 *   to change the escape character with a UESCAPE clause following the Unicode quoted identifier.
 *
 * TODO: these rules assume "" is still a valid escape sequence within a Unicode quoted identifier.
 */


UnicodeQuotedIdentifier
   : U '&' QuotedIdentifier
   ;
   // This is a Unicode quoted identifier which only contains valid characters but is not terminated

UnterminatedUnicodeQuotedIdentifier
   : U '&' UnterminatedQuotedIdentifier
   ;
   // This is a Unicode quoted identifier which is terminated but contains a \u0000 character

InvalidUnicodeQuotedIdentifier
   : U '&' InvalidQuotedIdentifier
   ;
   // This is a Unicode quoted identifier which is unterminated and contains a \u0000 character

InvalidUnterminatedUnicodeQuotedIdentifier
   : U '&' InvalidUnterminatedQuotedIdentifier
   ;
   //

   // CONSTANTS (4.1.2)

   //

   // String Constants (4.1.2.1)

StringConstant
   : UnterminatedStringConstant '\''
   ;

UnterminatedStringConstant
   : '\'' ('\'\'' | ~ '\'')*
   ;
   // String Constants with C-style Escapes (4.1.2.2)

BeginEscapeStringConstant
   : E '\'' -> more , pushMode (EscapeStringConstantMode)
   ;
   // String Constants with Unicode Escapes (4.1.2.3)

   //

   //   Note that escape sequences are never checked as part of this token due to the ability of users to change the escape

   //   character with a UESCAPE clause following the Unicode string constant.

   //

   // TODO: these rules assume '' is still a valid escape sequence within a Unicode string constant.

UnicodeEscapeStringConstant
   : UnterminatedUnicodeEscapeStringConstant '\''
   ;

UnterminatedUnicodeEscapeStringConstant
   : U '&' UnterminatedStringConstant
   ;
   // Dollar-quoted String Constants (4.1.2.4)

BeginDollarStringConstant
   : '$' Tag? '$'
   {pushTag();} -> pushMode (DollarQuotedStringMode)
   ;
/* "The tag, if any, of a dollar-quoted string follows the same rules as an
 * unquoted identifier, except that it cannot contain a dollar sign."
 */


fragment Tag
   : IdentifierStartChar StrictIdentifierChar*
   ;
   // Bit-strings Constants (4.1.2.5)

BinaryStringConstant
   : UnterminatedBinaryStringConstant '\''
   ;

UnterminatedBinaryStringConstant
   : B '\'' [01]*
   ;

InvalidBinaryStringConstant
   : InvalidUnterminatedBinaryStringConstant '\''
   ;

InvalidUnterminatedBinaryStringConstant
   : B UnterminatedStringConstant
   ;

HexadecimalStringConstant
   : UnterminatedHexadecimalStringConstant '\''
   ;

UnterminatedHexadecimalStringConstant
   : X '\'' [0-9a-fA-F]*
   ;

InvalidHexadecimalStringConstant
   : InvalidUnterminatedHexadecimalStringConstant '\''
   ;

InvalidUnterminatedHexadecimalStringConstant
   : X UnterminatedStringConstant
   ;
   // Numeric Constants (4.1.2.6)

Integral
   : Digits
   ;

NumericFail
   : Digits '..'
   {HandleNumericFail();}
   ;

Numeric
   : Digits '.' Digits? /*? replaced with + to solve problem with DOT_DOT .. but this surely must be rewriten */

   (E [+-]? Digits)?
   | '.' Digits (E [+-]? Digits)?
   | Digits E [+-]? Digits
   ;

fragment Digits
   : [0-9]+
   ;

PLSQLVARIABLENAME
   : ':' [a-zA-Z_] [a-zA-Z_0-9$]*
   ;

PLSQLIDENTIFIER
   : ':"' ('\\' . | '""' | ~ ('"' | '\\'))* '"'
   ;
   //

   // WHITESPACE (4.1)

   //

Whitespace
   : [ \t]+ -> channel (HIDDEN)
   ;

Newline
   : ('\r' '\n'? | '\n') -> channel (HIDDEN)
   ;
   //

   // COMMENTS (4.1.5)

   //

LineComment
   : '--' ~ [\r\n]* -> channel (HIDDEN)
   ;

BlockComment
   : ('/*' ('/'* BlockComment | ~ [/*] | '/'+ ~ [/*] | '*'+ ~ [/*])* '*'* '*/') -> channel (HIDDEN)
   ;

UnterminatedBlockComment
   : '/*' ('/'* BlockComment | // these characters are not part of special sequences in a block comment
   ~ [/*] | // handle / or * characters which are not part of /* or */ and do not appear at the end of the file
   ('/'+ ~ [/*] | '*'+ ~ [/*]))*
   // Handle the case of / or * characters at the end of the file, or a nested unterminated block comment
   ('/'+ | '*'+ | '/'* UnterminatedBlockComment)?
   // Optional assertion to make sure this rule is working as intended

   {
            UnterminatedBlockCommentDebugAssert();
   }
   ;
   //

   // META-COMMANDS

   //

   // http://www.postgresql.org/docs/9.3/static/app-psql.html

MetaCommand
   : '\\' (~ [\r\n\\"] | '"' ~ [\r\n"]* '"')* ('"' ~ [\r\n"]*)?
   ;

EndMetaCommand
   : '\\\\'
   ;
   //

   // ERROR

   //

   // Any character which does not match one of the above rules will appear in the token stream as an ErrorCharacter token.

   // This ensures the lexer itself will never encounter a syntax error, so all error handling may be performed by the

   // parser.

ErrorCharacter
   : .
   ;

mode EscapeStringConstantMode;
EscapeStringConstant
   : EscapeStringText '\'' -> mode (AfterEscapeStringConstantMode)
   ;

UnterminatedEscapeStringConstant
   : EscapeStringText
   // Handle a final unmatched \ character appearing at the end of the file
   '\\'? EOF
   ;

fragment EscapeStringText
   : ('\'\'' | '\\' ( // two-digit hex escapes are still valid when treated as single-digit escapes
   'x' [0-9a-fA-F] | 'u' [0-9a-fA-F] [0-9a-fA-F] [0-9a-fA-F] [0-9a-fA-F] | 'U' [0-9a-fA-F] [0-9a-fA-F] [0-9a-fA-F] [0-9a-fA-F] [0-9a-fA-F] [0-9a-fA-F] [0-9a-fA-F] [0-9a-fA-F] | // Any character other than the Unicode escapes can follow a backslash. Some have special meaning,

   // but that doesn't affect the syntax.
   ~ [xuU]) | ~ ['\\])*
   ;

InvalidEscapeStringConstant
   : InvalidEscapeStringText '\'' -> mode (AfterEscapeStringConstantMode)
   ;

InvalidUnterminatedEscapeStringConstant
   : InvalidEscapeStringText
   // Handle a final unmatched \ character appearing at the end of the file
   '\\'? EOF
   ;

fragment InvalidEscapeStringText
   : ('\'\'' | '\\' . | ~ ['\\])*
   ;

mode AfterEscapeStringConstantMode;
AfterEscapeStringConstantMode_Whitespace
   : Whitespace -> type (Whitespace) , channel (HIDDEN)
   ;

AfterEscapeStringConstantMode_Newline
   : Newline -> type (Newline) , channel (HIDDEN) , mode (AfterEscapeStringConstantWithNewlineMode)
   ;

AfterEscapeStringConstantMode_NotContinued
   :
   {} // intentionally empty
   -> skip , popMode
   ;

mode AfterEscapeStringConstantWithNewlineMode;
AfterEscapeStringConstantWithNewlineMode_Whitespace
   : Whitespace -> type (Whitespace) , channel (HIDDEN)
   ;

AfterEscapeStringConstantWithNewlineMode_Newline
   : Newline -> type (Newline) , channel (HIDDEN)
   ;

AfterEscapeStringConstantWithNewlineMode_Continued
   : '\'' -> more , mode (EscapeStringConstantMode)
   ;

AfterEscapeStringConstantWithNewlineMode_NotContinued
   :
   {} // intentionally empty
   -> skip , popMode
   ;

mode DollarQuotedStringMode;
DollarText
   : ~ '$'+
   //| '$'([0-9])+
   | // this alternative improves the efficiency of handling $ characters within a dollar-quoted string which are

   // not part of the ending tag.
   '$' ~ '$'*
   ;

EndDollarStringConstant
   : ('$' Tag? '$')
   {isTag()}?
   {popTag();} -> popMode
   ;

