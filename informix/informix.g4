/*
* Originally written by Gregorio Momm Mon Jan 5, 2004 18:17
*
* Ported to Antlr4 by Tom Everett
*
*/

grammar informix;

compilation_unit
   : databaseDeclaration? globalDeclaration? typeDeclarations? mainBlock functionOrReportDefinitions? EOF
   ;

identifier
   : IDENT
   ;

mainBlock
   : eol? MAIN eol typeDeclarations? mainStatements? END MAIN eol
   ;

mainStatements
   : (deferStatement | codeBlock | eol)+
   ;

deferStatement
   : DEFER (INTERRUPT | QUIT) eol
   ;

functionOrReportDefinitions
   : (reportDefinition | functionDefinition)+
   ;

returnStatement
   : RETURN variableOrConstantList?
   ;

functionDefinition
   : FUNCTION functionIdentifier parameterList? eol typeDeclarations codeBlock? END FUNCTION eol
   ;

parameterList
   : LPAREN parameterGroup* RPAREN
   ;

parameterGroup
   : identifier (COMMA identifier)*
   ;

globalDeclaration
   : GLOBALS (string | eol typeDeclarations END GLOBALS) eol
   ;

typeDeclarations
   : typeDeclaration+
   ;

typeDeclaration
   : DEFINE variableDeclaration (COMMA variableDeclaration)*
   ;

variableDeclaration
   : constantIdentifier (COMMA constantIdentifier)* type
   | constantIdentifier type (COMMA constantIdentifier type)*
   ;

type
   : typeIdentifier
   | indirectType
   | largeType
   | structuredType
   ;

indirectType
   : LIKE tableIdentifier DOT identifier
   ;

typeIdentifier
   : charType
   | numberType
   | timeType
   ;

largeType
   : TEXT
   | BYTE
   ;

numberType
   : (BIGINT | INTEGER | INT | SMALLINT | REAL | SMALLFLOAT)
   | (DECIMAL | DEC | NUMERIC | MONEY) (LPAREN numericConstant (COMMA numericConstant)? RPAREN)?
   | (FLOAT | DOUBLE) (LPAREN numericConstant RPAREN)?
   ;

charType
   : (VARCHAR | NVARCHAR) LPAREN numericConstant (COMMA numericConstant)? RPAREN
   | (CHAR | NCHAR | CHARACTER) (LPAREN numericConstant RPAREN)?
   ;

timeType
   : DATE
   | DATETIME datetimeQualifier
   | INTERVAL intervalQualifier
   ;

datetimeQualifier
   : YEAR TO yearQualifier
   | MONTH TO monthQualifier
   | DAY TO dayQualifier
   | HOUR TO hourQualifier
   | MINUTE TO minuteQualifier
   | SECOND TO secondQualifier
   | FRACTION TO fractionQualifier
   ;

intervalQualifier
   : YEAR (LPAREN numericConstant RPAREN)? TO yearQualifier
   | MONTH (LPAREN numericConstant RPAREN)? TO monthQualifier
   | DAY (LPAREN numericConstant RPAREN)? TO dayQualifier
   | HOUR (LPAREN numericConstant RPAREN)? TO hourQualifier
   | MINUTE (LPAREN numericConstant RPAREN)? TO minuteQualifier
   | SECOND (LPAREN numericConstant RPAREN)? TO secondQualifier
   | FRACTION TO fractionQualifier
   ;

unitType
   : yearQualifier
   ;

yearQualifier
   : YEAR
   | monthQualifier
   ;

monthQualifier
   : MONTH
   | dayQualifier
   ;

dayQualifier
   : DAY
   | hourQualifier
   ;

hourQualifier
   : HOUR
   | minuteQualifier
   ;

minuteQualifier
   : MINUTE
   | secondQualifier
   ;

secondQualifier
   : SECOND
   | fractionQualifier
   ;

fractionQualifier
   : FRACTION (LPAREN numericConstant RPAREN)?
   ;

structuredType
   : recordType
   | arrayType
   | dynArrayType
   ;

recordType
   : RECORD (eol (variableDeclaration (COMMA variableDeclaration)*) END RECORD | (LIKE tableIdentifier DOT STAR))
   ;

arrayIndexer
   : LBRACK numericConstant (COMMA numericConstant | COMMA numericConstant COMMA numericConstant)? RBRACK
   ;

arrayType
   : ARRAY arrayIndexer OF (recordType | typeIdentifier | largeType)
   ;

dynArrayType
   : DYNAMIC ARRAY WITH numericConstant DIMENSIONS OF (recordType | typeIdentifier)
   ;

string
   : STRING_LITERAL
   ;

statement
   : (label COLON)? unlabelledStatement
   ;

codeBlock
   : (statement | databaseDeclaration)+
   ;

label
   : identifier
   ;

unlabelledStatement
   : simpleStatement
   | structuredStatement
   ;

simpleStatement
   : assignmentStatement
   | procedureStatement
   | sqlStatements SEMI?
   | otherFGLStatement
   | menuInsideStatement
   | constructInsideStatement
   | displayInsideStatement
   | inputInsideStatement
   ;

runStatement
   : RUN (variable | string) (IN FORM MODE | IN LINE MODE)? (WITHOUT WAITING | RETURNING variable)?
   ;

assignmentStatement
   : LET variable EQUAL expression (COMMA expression)*
   ;

procedureStatement
   : CALL procedureIdentifier (LPAREN (actualParameter (COMMA actualParameter)*)? RPAREN)? (RETURNING variable (COMMA variable)*)?
   ;

procedureIdentifier
   : functionIdentifier
   ;

actualParameter
   : STAR
   | expression
   ;

gotoStatement
   : GOTO COLON? label eol
   ;

condition
   : (TRUE | FALSE)
   | logicalTerm (OR logicalTerm)*
   ;

logicalTerm
   : logicalFactor (AND logicalFactor)*
   ;

logicalFactor
   :
   // Added "prior" to a comparison expression to support use of a
   // condition in a connect_clause. (sqlExpression NOT? IN) sqlExpression NOT? IN expressionSet
   | (sqlExpression NOT? LIKE) sqlExpression NOT? LIKE sqlExpression (ESC QUOTED_STRING)?
   | (sqlExpression NOT? BETWEEN) sqlExpression NOT? BETWEEN sqlExpression AND sqlExpression
   | (sqlExpression IS NOT? NULL) sqlExpression IS NOT? NULL
   | quantifiedFactor quantifiedFactor
   | (NOT condition) NOT condition
   | LPAREN condition RPAREN
   | sqlExpression relationalOperator sqlExpression
   ;

quantifiedFactor
   : (sqlExpression relationalOperator (ALL | ANY)? subquery) sqlExpression relationalOperator (ALL | ANY)? subquery
   | (NOT? EXISTS subquery) NOT? EXISTS subquery
   | subquery
   ;

expressionSet
   : sqlExpression sqlExpression
   | subquery
   ;

subquery
   : LPAREN sqlSelectStatement RPAREN
   ;

sqlExpression
   : sqlTerm ((PLUS | MINUS) sqlTerm)*
   ;

sqlAlias
   : AS? identifier
   ;

sqlTerm
   : sqlFactor ((sqlMultiply | DIV | SLASH) sqlFactor)*
   ;

sqlMultiply
   : STAR
   ;

sqlFactor
   : sqlFactor2 (DOUBLEVERTBAR sqlFactor2)*
   ;

sqlFactor2
   : (sqlVariable (UNITS unitType)?) sqlVariable (UNITS unitType)?
   | (sqlLiteral (UNITS unitType)?) sqlLiteral (UNITS unitType)?
   | (groupFunction LPAREN (STAR | ALL | DISTINCT)? (sqlExpression (COMMA sqlExpression)*)? RPAREN) groupFunction LPAREN (STAR | ALL | DISTINCT)? (sqlExpression (COMMA sqlExpression)*)? RPAREN
   | (sqlFunction (LPAREN sqlExpression (COMMA sqlExpression)* RPAREN)) sqlFunction (LPAREN sqlExpression (COMMA sqlExpression)* RPAREN)
   | ((PLUS | MINUS) sqlExpression) (PLUS | MINUS) sqlExpression
   | (LPAREN sqlExpression RPAREN) LPAREN sqlExpression RPAREN
   | sqlExpressionList
   ;

sqlExpressionList
   : LPAREN sqlExpression (COMMA sqlExpression)+ RPAREN
   ;

sqlLiteral
   : unsignedConstant | string | NULL | FALSE | TRUE
   ;

sqlVariable
   : columnsTableId columnsTableId
   ;

sqlFunction
   : numberFunction
   | charFunction
   | dateFunction
   | otherFunction
   ;

dateFunction
   : YEAR
   | DATE
   | DAY
   | MONTH
   ;

numberFunction
   : MOD
   ;

charFunction
   : LENGTH
   ;

groupFunction
   : AVG
   | COUNT
   | MAX
   | MIN
   | SUM
   ;

otherFunction
   : (DECODE | NVL)
   | constantIdentifier
   ;

// This is not being used currently, but might be useful at some point.
sqlPseudoColumn
   : USER
   ;

relationalOperator
   : EQUAL | NOT_EQUAL | LE | LT | GE | GT | LIKE
   | NOT? MATCHES
   ;

ifCondition
   : TRUE | FALSE
   | ifCondition2 (relationalOperator ifCondition2)*
   ;

ifCondition2
   : ifLogicalTerm (OR ifLogicalTerm)*
   ;

ifLogicalTerm
   : ifLogicalFactor (AND ifLogicalFactor)*
   ;

expression
   : simpleExpression (CLIPPED | USING string)*
   ;

ifLogicalFactor
   :
   // Added "prior" to a comparison expression to support use of a
   // condition in a connect_clause. (simpleExpression IS NOT? NULL) simpleExpression IS NOT? NULL
   | (NOT ifCondition) NOT ifCondition
   | LPAREN ifCondition RPAREN
   | simpleExpression simpleExpression
   ;

simpleExpression
   : sign? term (addingOperator term)*
   ;

addingOperator
   : PLUS
   | MINUS
   ;

term
   : factor (multiplyingOperator factor)*
   ;

multiplyingOperator
   : STAR
   | SLASH
   | DIV
   | MOD
   ;

factor
   : (GROUP? functionDesignator | variable | constant | LPAREN expression RPAREN | NOT factor) (UNITS unitType)?
   ;

functionDesignator
   : functionIdentifier (LPAREN (actualParameter (COMMA actualParameter)*)? RPAREN)?
   ;

functionIdentifier
   : (DAY | YEAR | MONTH | TODAY | WEEKDAY | MDY | COLUMN | SUM | COUNT | AVG | MIN | MAX | EXTEND | DATE | TIME | INFIELD | PREPARE)
   | constantIdentifier
   ;

unsignedConstant
   : unsignedNumber
   | string
   | constantIdentifier
   | NULL
   ;

constant
   : numericConstant
   | constantIdentifier
   | sign identifier
   | string
   ;

numericConstant
   : unsignedNumber
   | sign unsignedNumber
   ;

variable
   : entireVariable
   | componentVariable
   ;

entireVariable
   : variableIdentifier
   ;

variableIdentifier
   : constantIdentifier
   ;

indexingVariable
   : LBRACK expression (COMMA expression)* RBRACK
   ;

/*
thruNotation
    :
     ( (THROUGH |THRU) (SAME DOT)? identifier )?
    ;
*/
componentVariable
   : (recordVariable indexingVariable?) ((DOT STAR) | (DOT componentVariable ((THROUGH | THRU) componentVariable)?))?
   ;

recordVariable
   : constantIdentifier
   ;

fieldIdentifier
   : constantIdentifier
   ;

structuredStatement
   : conditionalStatement
   | repetetiveStatement
   ;

conditionalStatement
   : ifStatement
   | caseStatement
   ;

ifStatement
   : IF ifCondition THEN codeBlock? (ELSE codeBlock?)? END IF
   ;

repetetiveStatement
   : whileStatement
   | forEachStatement
   | forStatement
   ;

whileStatement
   : WHILE ifCondition codeBlock? END WHILE
   ;

forStatement
   : FOR controlVariable EQUAL forList (STEP numericConstant)? eol codeBlock? END FOR eol
   ;

forList
   : initialValue TO finalValue
   ;

controlVariable
   : identifier
   ;

initialValue
   : expression
   ;

finalValue
   : expression
   ;

forEachStatement
   : FOREACH identifier (USING variableList)? (INTO variableList)? (WITH REOPTIMIZATION)? eol codeBlock? END FOREACH eol
   ;

variableList
   : variable (COMMA variable)*
   ;

variableOrConstantList
   : expression (COMMA expression)*
   ;

caseStatement
   : CASE expression (WHEN expression codeBlock?)* (OTHERWISE codeBlock?)? END CASE
   | CASE (WHEN ifCondition codeBlock)* (OTHERWISE codeBlock)? END CASE
   ;

otherFGLStatement
   : otherProgramFlowStatement
   | otherStorageStatement
   | reportStatement
   | screenStatement
   ;

otherProgramFlowStatement
   : runStatement
   | gotoStatement
   | SLEEP expression
   | exitStatements
   | continueStatements
   | returnStatement
   ;

exitTypes
   : FOREACH
   | FOR
   | CASE
   | CONSTRUCT
   | DISPLAY
   | INPUT
   | MENU
   | REPORT
   | WHILE
   ;

exitStatements
   : EXIT exitTypes
   | EXIT PROGRAM (LPAREN expression RPAREN | expression)?
   ;

continueStatements
   : CONTINUE exitTypes eol
   ;

otherStorageStatement
   : ALLOCATE ARRAY identifier arrayIndexer
   | LOCATE variableList IN (MEMORY | FILE (variable | string)?)
   | DEALLOCATE ARRAY identifier
   | RESIZE ARRAY identifier arrayIndexer
   | FREE variable (COMMA variable)*
   | INITIALIZE variable (COMMA variable)* (TO NULL | LIKE expression (COMMA expression)*)
   | VALIDATE variable (COMMA variable)* LIKE expression (COMMA expression)*
   ;

printExpressionItem
   : COLUMN expression
   | (PAGENO | LINENO)
   | BYTE variable
   | TEXT variable
   | expression (SPACE | SPACES)? (WORDWRAP (RIGHT MARGIN numericConstant)?)?
   ;

printExpressionList
   : printExpressionItem (COMMA printExpressionItem)*
   ;

reportStatement
   : START REPORT constantIdentifier (TO (expression | PIPE expression | PRINTER))? (WITH ((LEFT MARGIN numericConstant) | (RIGHT MARGIN numericConstant) | (TOP MARGIN numericConstant) | (BOTTOM MARGIN numericConstant) | (PAGE LENGTH numericConstant) | (TOP OF PAGE string))*)?
   | TERMINATE REPORT constantIdentifier
   | FINISH REPORT constantIdentifier
   | PAUSE string?
   | NEED expression LINES
   | PRINT (printExpressionList? SEMI? | FILE string)?
   | SKIP2 (expression (LINE | LINES) | TO TOP OF PAGE)
   | OUTPUT TO REPORT constantIdentifier LPAREN (expression (COMMA expression)*)? RPAREN
   ;

fieldName
   : ((identifier (LBRACK numericConstant RBRACK)?) DOT)? identifier
   | (identifier (LBRACK numericConstant RBRACK)?) DOT (STAR | identifier thruNotation?)
   ;

thruNotation
   : (THROUGH | THRU) (SAME DOT)? identifier
   ;

fieldList
   : expression (COMMA expression)*
   ;

keyList
   : expression (COMMA expression)*
   ;

constructEvents
   : BEFORE CONSTRUCT
   | AFTER CONSTRUCT
   | BEFORE FIELD fieldList
   | AFTER FIELD fieldList
   | ON KEY LPAREN keyList RPAREN
   ;

constructInsideStatement
   : NEXT FIELD (fieldName | NEXT | PREVIOUS) eol
   | CONTINUE CONSTRUCT eol
   | EXIT CONSTRUCT eol
   ;

specialAttribute
   : REVERSE
   | BLINK
   | UNDERLINE
   ;

attribute
   : (BLACK | BLUE | CYAN | GREEN | MAGENTA | RED | WHITE | YELLOW | BOLD | DIM | NORMAL | INVISIBLE)? specialAttribute (COMMA specialAttribute)*
   ;

attributeList
   : (ATTRIBUTE | ATTRIBUTES) LPAREN attribute RPAREN
   ;

constructGroupStatement
   : constructEvents codeBlock+
   ;

constructStatement
   : CONSTRUCT (BY NAME variable ON columnsList | variable ON columnsList FROM fieldList) attributeList? (HELP numericConstant)? (constructGroupStatement+ END CONSTRUCT)?
   ;

displayArrayStatement
   : DISPLAY ARRAY expression TO expression attributeList? displayEvents* (END DISPLAY)?
   ;

displayInsideStatement
   : CONTINUE DISPLAY
   | EXIT DISPLAY
   ;

displayEvents
   : ON KEY LPAREN keyList RPAREN codeBlock+
   ;

displayStatement
   : DISPLAY (BY NAME (expression (COMMA expression)*) | (expression (COMMA expression)*) (TO fieldList | AT expression COMMA expression)?) attributeList? eol
   ;

errorStatement
   : ERROR expression (COMMA expression)* attributeList?
   ;

messageStatement
   : MESSAGE expression (COMMA expression)* attributeList?
   ;

promptStatement
   : PROMPT expression (COMMA expression)* attributeList? FOR CHAR? variable (HELP numericConstant)? attributeList? ((ON KEY LPAREN keyList RPAREN codeBlock?)* END PROMPT)?
   ;

inputEvents
   : (BEFORE | AFTER) (INPUT | ROW | INSERT | DELETE)
   | BEFORE FIELD fieldList
   | AFTER FIELD fieldList
   | ON KEY LPAREN keyList RPAREN
   ;

inputInsideStatement
   : NEXT FIELD (fieldName | (NEXT | PREVIOUS))
   | (CONTINUE INPUT | EXIT INPUT)
   ;

inputGroupStatement
   : inputEvents codeBlock*
   ;

inputStatement
   : INPUT (BY NAME expression (COMMA expression)* (WITHOUT DEFAULTS)? | expression (COMMA expression)* (WITHOUT DEFAULTS)? FROM fieldList) attributeList? (HELP numericConstant)? (inputGroupStatement+ END INPUT)?
   ;

inputArrayStatement
   : INPUT ARRAY expression (WITHOUT DEFAULTS)? FROM expression (COMMA expression)* (HELP numericConstant)? attributeList? (inputGroupStatement+ END INPUT)?
   ;

menuEvents
   : BEFORE MENU
   | COMMAND ((KEY LPAREN keyList RPAREN)? expression expression? (HELP numericConstant)?)
   ;

menuInsideStatement
   : NEXT OPTION (expression | ALL) (COMMA expression)*
   | SHOW OPTION (expression | ALL) (COMMA expression)*
   | HIDE OPTION (expression | ALL) (COMMA expression)*
   | CONTINUE MENU
   | EXIT MENU
   ;

menuGroupStatement
   : menuEvents codeBlock?
   ;

menuStatement
   : MENU expression menuGroupStatement* END MENU
   ;

reservedLinePosition
   : FIRST (PLUS numericConstant)?
   | numericConstant
   | LAST (MINUS numericConstant)?
   ;

specialWindowAttribute
   : (BLACK | BLUE | CYAN | GREEN | MAGENTA | RED | WHITE | YELLOW | BOLD | DIM | NORMAL | INVISIBLE)
   | REVERSE
   | BORDER
   | (PROMPT | FORM | MENU | MESSAGE) LINE reservedLinePosition
   | COMMENT LINE (reservedLinePosition | OFF)
   ;

windowAttribute
   : specialWindowAttribute (COMMA specialWindowAttribute)*
   ;

windowAttributeList
   : (ATTRIBUTE | ATTRIBUTES) LPAREN windowAttribute RPAREN
   ;

optionStatement
   : (MESSAGE LINE expression | PROMPT LINE expression | MENU LINE expression | COMMENT LINE expression | ERROR LINE expression | FORM LINE expression | INPUT (WRAP | NO WRAP) | INSERT KEY expression | DELETE KEY expression | NEXT KEY expression | PREVIOUS KEY expression | ACCEPT KEY expression | HELP FILE expression | HELP KEY expression | INPUT attributeList | DISPLAY attributeList | SQL INTERRUPT (ON | OFF) | FIELD ORDER (CONSTRAINED | UNCONSTRAINED))
   ;

optionsStatement
   : OPTIONS optionStatement (COMMA optionStatement)*
   ;

screenStatement
   : CLEAR (FORM | WINDOW identifier | WINDOW? SCREEN | fieldList)
   | CLOSE WINDOW identifier eol
   | CLOSE FORM identifier eol
   | constructStatement
   | CURRENT WINDOW IS (SCREEN | identifier) eol
   | displayStatement
   | displayArrayStatement
   | DISPLAY FORM identifier attributeList? eol
   | errorStatement
   | messageStatement
   | promptStatement
   | inputStatement
   | inputArrayStatement
   | menuStatement
   | OPEN FORM expression FROM expression
   | OPEN WINDOW expression AT expression COMMA expression (WITH FORM expression | WITH expression ROWS COMMA expression COLUMNS) windowAttributeList?
   | optionsStatement
   | SCROLL fieldList (COMMA fieldList)* (UP | DOWN) (BY numericConstant)?
   ;

sqlStatements
   : cursorManipulationStatement
   | dataDefinitionStatement
   | dataManipulationStatement
   | dynamicManagementStatement
   | queryOptimizationStatement
   | dataIntegrityStatement
   | clientServerStatement
   ;

cursorManipulationStatement
   : CLOSE cursorName eol
   | DECLARE cursorName (CURSOR (WITH HOLD)? FOR (sqlSelectStatement (FOR UPDATE (OF columnsList)?)? | sqlInsertStatement | statementId) | SCROLL CURSOR (WITH HOLD)? FOR (sqlSelectStatement | statementId))
   | FETCH (NEXT | (PREVIOUS | PRIOR) | FIRST | LAST | CURRENT | RELATIVE expression | ABSOLUTE expression)? cursorName (INTO variableList)?
   | FLUSH cursorName eol
   | OPEN cursorName (USING variableList)?
   | PUT cursorName (FROM variableOrConstantList)?
   ;

columnsList
   : columnsTableId (COMMA columnsTableId)*
   ;

statementId
   : constantIdentifier
   ;

cursorName
   : identifier
   ;

dataType
   : type
   ;

columnItem
   : constantIdentifier (dataType | (BYTE | TEXT) (IN (TABLE | constantIdentifier))?) (NOT NULL)?
   | UNIQUE LPAREN (constantIdentifier (COMMA constantIdentifier)*)? RPAREN (CONSTRAINT constantIdentifier)?
   ;

dataDefinitionStatement
   : DROP TABLE constantIdentifier
   | CREATE TEMP? TABLE constantIdentifier LPAREN columnItem (COMMA columnItem)* RPAREN (WITH NO LOG)? (IN constantIdentifier)? (EXTENT SIZE numericConstant)? (NEXT SIZE numericConstant)? (LOCK MODE LPAREN (PAGE | ROW) RPAREN)?
   | CREATE UNIQUE? CLUSTER? INDEX constantIdentifier ON constantIdentifier LPAREN constantIdentifier (ASC | DESC)? (COMMA constantIdentifier (ASC | DESC)?)* RPAREN
   | DROP INDEX constantIdentifier
   ;

dataManipulationStatement
   : sqlInsertStatement
   | sqlDeleteStatement
   | sqlSelectStatement
   | sqlUpdateStatement
   | sqlLoadStatement
   | sqlUnLoadStatement
   ;

sqlSelectStatement
   : mainSelectStatement
   ;

columnsTableId
   : STAR
   | (tableIdentifier indexingVariable?) (DOT STAR | DOT columnsTableId)?
   ;

selectList
   : (sqlExpression sqlAlias? (COMMA sqlExpression sqlAlias?)*)
   ;

headSelectStatement
   : SELECT (ALL | (DISTINCT | UNIQUE))? selectList
   ;

tableQualifier
   : constantIdentifier COLON
   | constantIdentifier ATSYMBOL constantIdentifier COLON
   | string
   ;

tableIdentifier
   : tableQualifier? constantIdentifier
   ;

fromTable
   : OUTER? tableIdentifier sqlAlias?
   ;

tableExpression
   : simpleSelectStatement
   ;

fromSelectStatement
   : FROM (fromTable | LPAREN tableExpression RPAREN sqlAlias?) (COMMA (fromTable | LPAREN tableExpression RPAREN sqlAlias?))*
   ;

aliasName
   : identifier
   ;

mainSelectStatement
   : headSelectStatement (INTO variableList)? fromSelectStatement whereStatement? groupByStatement? havingStatement? unionSelectStatement? orderbyStatement? (INTO TEMP identifier)? (WITH NO LOG)?
   ;

unionSelectStatement
   : (UNION ALL? simpleSelectStatement)
   ;

simpleSelectStatement
   : headSelectStatement fromSelectStatement whereStatement? groupByStatement? havingStatement? unionSelectStatement?
   ;

whereStatement
   : WHERE condition
   ;

groupByStatement
   : GROUP BY variableOrConstantList
   ;

havingStatement
   : HAVING condition
   ;

orderbyColumn
   : expression (ASC | DESC)?
   ;

orderbyStatement
   : ORDER BY orderbyColumn (COMMA orderbyColumn)*
   ;

sqlLoadStatement
   : LOAD FROM (variable | string) (DELIMITER (variable | string))? (INSERT INTO tableIdentifier (LPAREN columnsList RPAREN)? | sqlInsertStatement) eol
   ;

sqlUnLoadStatement
   : UNLOAD TO (variable | string) (DELIMITER (variable | string))? sqlSelectStatement eol
   ;

sqlInsertStatement
   : INSERT INTO tableIdentifier (LPAREN columnsList RPAREN)? (VALUES LPAREN expression (COMMA expression)* RPAREN | simpleSelectStatement)
   ;

sqlUpdateStatement
   : UPDATE tableIdentifier SET ((columnsTableId EQUAL expression (COMMA columnsTableId EQUAL expression)*) | ((LPAREN columnsList RPAREN | (aliasName DOT)? STAR) EQUAL (LPAREN expression (COMMA expression)* RPAREN | (aliasName DOT)? STAR))) (WHERE (condition | CURRENT OF cursorName))?
   ;

sqlDeleteStatement
   : DELETE FROM tableIdentifier (WHERE (condition | CURRENT OF cursorName))? eol
   ;

actualParameterList
   : actualParameter (COMMA actualParameter)*
   ;

dynamicManagementStatement
   : PREPARE cursorName FROM expression
   | EXECUTE cursorName (USING variableList)?
   | FREE (cursorName | statementId)
   | LOCK TABLE expression IN (SHARE | EXCLUSIVE) MODE
   ;

queryOptimizationStatement
   : UPDATE STATISTICS (FOR TABLE tableIdentifier)?
   | SET LOCK MODE TO (WAIT SECONDS? | NOT WAIT)
   | SET EXPLAIN (ON | OFF)
   | SET ISOLATION TO (CURSOR STABILITY | (DIRTY | COMMITTED | REPEATABLE) READ)
   | SET BUFFERED? LOG
   ;

databaseDeclaration
   : DATABASE (constantIdentifier (ATSYMBOL constantIdentifier)?) EXCLUSIVE? SEMI?
   ;

clientServerStatement
   : CLOSE DATABASE
   ;

dataIntegrityStatement
   : wheneverStatement
   | BEGIN WORK
   | COMMIT WORK
   | ROLLBACK WORK
   ;

wheneverStatement
   : WHENEVER wheneverType wheneverFlow eol
   ;

wheneverType
   : NOT FOUND
   | ANY? (SQLERROR | ERROR)
   | (SQLWARNING | WARNING)
   ;

wheneverFlow
   : (CONTINUE | STOP)
   | CALL identifier
   | (GO TO | GOTO) COLON? identifier
   ;

reportDefinition
   : REPORT identifier parameterList? typeDeclarations? outputReport? (ORDER EXTERNAL? BY variableList)? formatReport? END REPORT
   ;

outputReport
   : OUTPUT (REPORT TO (string | PIPE string | PRINTER))? ((LEFT MARGIN numericConstant) | (RIGHT MARGIN numericConstant) | (TOP MARGIN numericConstant) | (BOTTOM MARGIN numericConstant) | (PAGE LENGTH numericConstant) | (TOP OF PAGE string))*
   ;

formatReport
   : FORMAT (EVERY ROW | ((FIRST? PAGE HEADER | PAGE TRAILER | ON (EVERY ROW | LAST ROW) | (BEFORE | AFTER) GROUP OF variable) codeBlock)+)
   ;

eol
   : EOL
   ;

unsignedNumber
   : unsignedInteger
   | unsignedReal
   ;

unsignedInteger
   : NUM_INT
   ;

unsignedReal
   : NUM_REAL
   ;

sign
   : PLUS
   | MINUS
   ;

constantIdentifier
   : (ACCEPT | ASCII | COUNT | CURRENT | FALSE | FIRST | FOUND | GROUP | HIDE | INDEX | INT_FLAG | INTERRUPT | LAST | LENGTH | LINENO | MDY | NO | NOT | NOTFOUND | NULL | PAGENO | REAL | SIZE | SQL | STATUS | TEMP | TIME | TODAY | TRUE | USER | WAIT | WEEKDAY | WORK)
   | identifier
   ;


fragment A
   : ('a' | 'A')
   ;


fragment B
   : ('b' | 'B')
   ;


fragment C
   : ('c' | 'C')
   ;


fragment D
   : ('d' | 'D')
   ;


fragment E
   : ('e' | 'E')
   ;


fragment F
   : ('f' | 'F')
   ;


fragment G
   : ('g' | 'G')
   ;


fragment H
   : ('h' | 'H')
   ;


fragment I
   : ('i' | 'I')
   ;


fragment J
   : ('j' | 'J')
   ;


fragment K
   : ('k' | 'K')
   ;


fragment L
   : ('l' | 'L')
   ;


fragment M
   : ('m' | 'M')
   ;


fragment N
   : ('n' | 'N')
   ;


fragment O
   : ('o' | 'O')
   ;


fragment P
   : ('p' | 'P')
   ;


fragment Q
   : ('q' | 'Q')
   ;


fragment R
   : ('r' | 'R')
   ;


fragment S
   : ('s' | 'S')
   ;


fragment T
   : ('t' | 'T')
   ;


fragment U
   : ('u' | 'U')
   ;


fragment V
   : ('v' | 'V')
   ;


fragment W
   : ('w' | 'W')
   ;


fragment X
   : ('x' | 'X')
   ;


fragment Y
   : ('y' | 'Y')
   ;


fragment Z
   : ('z' | 'Z')
   ;


ABSOLUTE
   : A B S O L U T E
   ;


AFTER
   : A F T E R
   ;


ACCEPT
   : A C C E P T
   ;


AGGREGATE
   : A G G R E G A T E
   ;


ALLOCATE
   : A L L O C A T E
   ;


ALL
   : A L L
   ;


ALL_ROWS
   : A L L '_' R O W S
   ;


AND
   : A N D
   ;


ANY
   : A N Y
   ;


AS
   : A S
   ;


ASC
   : A S C
   ;


ASCII
   : A S C I I
   ;


AT
   : A T
   ;


ATTRIBUTE
   : A T T R I B U T E
   ;


ATTRIBUTES
   : A T T R I B U T E S
   ;


AVERAGE
   : A V E R A G E
   ;


AVG
   : A V G
   ;


ARRAY
   : A R R A Y
   ;


BEFORE
   : B E F O R E
   ;


BEGIN
   : B E G I N
   ;

//TRANSACTION CONTROL

BETWEEN
   : B E T W E E N
   ;


BIGINT
   : B I G I N T
   ;


BLACK
   : B L A C K
   ;


BLINK
   : B L I N K
   ;


BLUE
   : B L U E
   ;


BOLD
   : B O L D
   ;


BORDER
   : B O R D E R
   ;


BOTTOM
   : B O T T O M
   ;


BUFFERED
   : B U F F E R E D
   ;


BY
   : B Y
   ;


BYTE
   : B Y T E
   ;


CACHE
   : C A C H E
   ;


CALL
   : C A L L
   ;


CASE
   : C A S E
   ;


CHAR
   : C H A R
   ;


CHARARACTER
   : C H A R A R A C T E R
   ;


CHAR_LENGTH
   : C H A R '_' L E N G T H
   ;


CHECK
   : C H E C K
   ;


CLEAR
   : C L E A R
   ;


CLIPPED
   : C L I P P E D
   ;


CLOSE
   : C L O S E
   ;


CLUSTER
   : C L U S T E R
   ;


COLUMN
   : C O L U M N
   ;


COLUMNS
   : C O L U M N S
   ;


COMMAND
   : C O M M A N D
   ;


COMMENT
   : C O M M E N T
   ;


COMMIT
   : C O M M I T
   ;


COMMITTED
   : C O M M I T T E D
   ;


CONSTANT
   : C O N S T A N T
   ;


CONSTRAINED
   : C O N S T R A I N E D
   ;


CONSTRAINT
   : C O N S T R A I N T
   ;


CONSTRUCT
   : C O N S T R U C T
   ;


CONTINUE
   : C O N T I N U E
   ;


COUNT
   : C O U N T
   ;


COPY
   : C O P Y
   ;


CRCOLS
   : C R C O L S
   ;


CREATE
   : C R E A T E
   ;


CURRENT
   : C U R R E N T
   ;


CURSOR
   : C U R S O R
   ;


CYAN
   : C Y A N
   ;


DATABASE
   : D A T A B A S E
   ;


DATE
   : D A T E
   ;


DATETIME
   : D A T E T I M E
   ;


DAY
   : D A Y
   ;


DEALLOCATE
   : D E A L L O C A T E
   ;


DEC
   : D E C
   ;


DECIMAL
   : D E C I M A L
   ;


DECODE
   : D E C O D E
   ;


DECLARE
   : D E C L A R E
   ;


DEFAULT
   : D E F A U L T
   ;


DEFAULTS
   : D E F A U L T S
   ;


DEFER
   : D E F E R
   ;


DEFINE
   : D E F I N E
   ;


DELETE
   : D E L E T E
   ;

//SQL

DELIMITER
   : D E L I M I T E R
   ;

//SQL

DESC
   : D E S C
   ;


DIM
   : D I M
   ;


DIMENSIONS
   : D I M E N S I O N S
   ;


DIRTY
   : D I R T Y
   ;


DISPLAY
   : D I S P L A Y
   ;


DISTINCT
   : D I S T I N C T
   ;


DO
   : D O
   ;


DOUBLE
   : D O U B L E
   ;


DOWN
   : D O W N
   ;


DROP
   : D R O P
   ;


DYNAMIC
   : D Y N A M I C
   ;


ELSE
   : E L S E
   ;


END
   : E N D
   ;


ERROR
   : E R R O R
   ;


ESCAPE
   : E S C A P E
   ;


EVERY
   : E V E R Y
   ;


EXCLUSIVE
   : E X C L U S I V E
   ;


EXEC
   : E X E C
   ;


EXECUTE
   : E X E C U T E
   ;


EXIT
   : E X I T
   ;


EXISTS
   : E X I S T S
   ;


EXPLAIN
   : E X P L A I N
   ;


EXTEND
   : E X T E N D
   ;


EXTENT
   : E X T E N T
   ;


EXTERNAL
   : E X T E R N A L
   ;


FALSE
   : F A L S E
   ;


FETCH
   : F E T C H
   ;


FIELD
   : F I E L D
   ;


FIELD_TOUCHED
   : F I E L D '_' T O U C H E D
   ;


FILE
   : F I L E
   ;


FINISH
   : F I N I S H
   ;


FIRST
   : F I R S T
   ;


FIRST_ROWS
   : F I R S T '_' R O W S
   ;


FLOAT
   : F L O A T
   ;


FLUSH
   : F L U S H
   ;


FOR
   : F O R
   ;


FORM
   : F O R M
   ;


FORMAT
   : F O R M A T
   ;


FORMONLY
   : F O R M O N L Y
   ;


FOREACH
   : F O R E A C H
   ;


FOUND
   : F O U N D
   ;


FRACTION
   : F R A C T I O N
   ;


FREE
   : F R E E
   ;


FROM
   : F R O M
   ;

//SQL & PREPARE

FUNCTION
   : F U N C T I O N
   ;


GET_FLDBUF
   : G E T F L D B U F
   ;


GLOBALS
   : G L O B A L S
   ;


GO
   : G O
   ;


GOTO
   : G O T O
   ;


GREEN
   : G R E E N
   ;


GROUP
   : G R O U P
   ;

//SQL

HAVING
   : H A V I N G
   ;

//SQL

HEADER
   : H E A D E R
   ;


HELP
   : H E L P
   ;


HIDE
   : H I D E
   ;


HOLD
   : H O L D
   ;

//CURSOR

HOUR
   : H O U R
   ;

//CURSOR

IF
   : I F
   ;


IN
   : I N
   ;

//SQL

INNER
   : I N N E R
   ;

//SQL

INDEX
   : I N D E X
   ;

//SQL

INDICATOR
   : I N D I C A T O R
   ;

//SQL

INFIELD
   : I N F I E L D
   ;


INITIALIZE
   : I N I T I A L I Z E
   ;


INPUT
   : I N P U T
   ;


INSERT
   : I N S E R T
   ;

//SQL

INSTRUCTIONS
   : I N S T R U C T I O N S
   ;


INTO
   : I N T O
   ;

//SQL & CURSOR 

INT
   : I N T
   ;


INT_FLAG
   : I N T '_' F L A G
   ;


INTEGER
   : I N T E G E R
   ;


INTERRUPT
   : I N T E R R U P T
   ;


INTERVAL
   : I N T E R V A L
   ;


INVISIBLE
   : I N V I S I B L E
   ;


IS
   : I S
   ;


ISNULL
   : I S N U L L
   ;


ISOLATION
   : I S O L A T I O N
   ;


JOIN
   : J O I N
   ;


KEY
   : K E Y
   ;


LABEL
   : L A B E L
   ;


LAST
   : L A S T
   ;


LEFT
   : L E F T
   ;


LENGTH
   : L E N G T H
   ;


LET
   : L E T
   ;


LIKE
   : L I K E
   ;


LINE
   : L I N E
   ;


LINENO
   : L I N E N O
   ;


LINES
   : L I N E S
   ;


LOAD
   : L O A D
   ;


LOCATE
   : L O C A T E
   ;


LOCK
   : L O C K
   ;


LOG
   : L O G
   ;


LONG
   : L O N G
   ;


MAGENTA
   : M A G E N T A
   ;


MATCHES
   : M A T C H E S
   ;


MENU
   : M E N U
   ;


MESSAGE
   : M E S S A G E
   ;


MAIN
   : M A I N
   ;


MARGIN
   : M A R G I N
   ;


MAX
   : M A X
   ;


MDY
   : M D Y
   ;


MIN
   : M I N
   ;


MINUTE
   : M I N U T E
   ;


MOD
   : M O D
   ;


MODE
   : M O D E
   ;


MODULE
   : M O D U L E
   ;


MONTH
   : M O N T H
   ;


MONEY
   : M O N E Y
   ;


NCHAR
   : N C H A R
   ;


NAME
   : N A M E
   ;


NEED
   : N E E D
   ;


NEXT
   : N E X T
   ;


NEW
   : N E W
   ;


NORMAL
   : N O R M A L
   ;


NO
   : N O
   ;


NOT
   : N O T
   ;


NOTFOUND
   : N O T F O U N D
   ;


NOW
   : N O W
   ;


NUMERIC
   : N U M E R I C
   ;


NULL
   : N U L L
   ;


NVARCHAR
   : N V A R C H A R
   ;

//SQL

NVL
   : N V L
   ;


OF
   : O F
   ;


OFF
   : O F F
   ;


ON
   : O N
   ;


OPEN
   : O P E N
   ;


OPTION
   : O P T I O N
   ;


OPTIONS
   : O P T I O N S
   ;


OR
   : O R
   ;


ORD
   : O R D
   ;


ORDER
   : O R D E R
   ;

//SQL

OUTPUT
   : O U T P U T
   ;


OUTER
   : O U T E R
   ;

//SQL

OTHERWISE
   : O T H E R W I S E
   ;


PAGE
   : P A G E
   ;


PAGENO
   : P A G E N O
   ;


PAUSE
   : P A U S E
   ;


PERCENT
   : P E R C E N T
   ;


PIPE
   : P I P E
   ;


PRECISION
   : P R E C I S I O N
   ;


PREPARE
   : P R E P A R E
   ;


PREVIOUS
   : P R E V I O U S
   ;


PRINT
   : P R I N T
   ;


PRINTER
   : P R I N T E R
   ;


PROGRAM
   : P R O G R A M
   ;


PROMPT
   : P R O M P T
   ;


PUT
   : P U T
   ;


QUIT
   : Q U I T
   ;


QUIT_FLAG
   : Q U I T '_' F L A G
   ;


RECORD
   : R E C O R D
   ;


REAL
   : R E A L
   ;


READ
   : R E A D
   ;


RED
   : R E D
   ;


RELATIVE
   : R E L A T I V E
   ;


REMOVE
   : R E M O V E
   ;


REOPTIMIZATION
   : R E O P T I M I Z A T I O N
   ;


REPEATABLE
   : R E P E A T A B L E
   ;


REPEAT
   : R E P E A T
   ;


REPORT
   : R E P O R T
   ;


RESIZE
   : R E S I Z E
   ;


RETURN
   : R E T U R N
   ;


RETURNING
   : R E T U R N I N G
   ;


REVERSE
   : R E V E R S E
   ;


RIGHT
   : R I G H T
   ;


ROLLBACK
   : R O L L B A C K
   ;


ROW
   : R O W
   ;


ROWS
   : R O W S
   ;


RUN
   : R U N
   ;


SCREEN
   : S C R E E N
   ;


SCROLL
   : S C R O L L
   ;


SECOND
   : S E C O N D
   ;


SKIP2
   : S K I P
   ;


SELECT
   : S E L E C T
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


SIZE
   : S I Z E
   ;


SLEEP
   : S L E E P
   ;


SMALLFLOAT
   : S M A L L F L O A T
   ;


SMALLINT
   : S M A L L I N T
   ;


SPACE
   : S P A C E
   ;


SPACES
   : S P A C E S
   ;


SQL
   : S Q L
   ;


SQLERROR
   : S Q L E R R O R
   ;


SQLWARNING
   : S Q L W A R N I N G
   ;


START
   : S T A R T
   ;


STABILITY
   : S T A B I L I T Y
   ;


STATISTICS
   : S T A T I S T I C S
   ;


STATUS
   : S T A T U S
   ;


STOP
   : S T O P
   ;


SUM
   : S U M
   ;


TABLE
   : T A B L E
   ;


TABLES
   : T A B L E S
   ;


TERMINATE
   : T E R M I N A T E
   ;


TEMP
   : T E M P
   ;


TEXT
   : T E X T
   ;


THEN
   : T H E N
   ;


THROUGH
   : T H R O U G H
   ;


THRU
   : T H R U
   ;


TIME
   : T I M E
   ;


TO
   : T O
   ;


TODAY
   : T O D A Y
   ;


TOP
   : T O P
   ;


TRAILER
   : T R A I L E R
   ;


TRUE
   : T R U E
   ;


TYPE
   : T Y P E
   ;


UNCONSTRAINED
   : U N C O N S T R A I N E D
   ;


UNDERLINE
   : U N D E R L I N E
   ;


UNION
   : U N I O N
   ;


UNIQUE
   : U N I Q U E
   ;


UNITS
   : U N I T S
   ;


UNLOAD
   : U N L O A D
   ;


UP
   : U P
   ;


UPDATE
   : U P D A T E
   ;

//SQL

USER
   : U S E R
   ;


USING
   : U S I N G
   ;


VALIDATE
   : V A L I D A T E
   ;


VALUES
   : V A L U E S
   ;

//SQL

VARCHAR
   : V A R C H A R
   ;

//SQL

WEEKDAY
   : W E E K D A Y
   ;


VIEW
   : V I E W
   ;

//SQL

WAIT
   : W A I T
   ;


WAITING
   : W A I T I N G
   ;


WARNING
   : W A R N I N G
   ;


WHEN
   : W H E N
   ;


WHENEVER
   : W H E N E V E R
   ;


WHERE
   : W H E R E
   ;

//SQL

WHILE
   : W H I L E
   ;


WHITE
   : W H I T E
   ;


WITH
   : W I T H
   ;


WITHOUT
   : W I T H O U T
   ;


WINDOW
   : W I N D O W
   ;


WORDWRAP
   : W O R D W R A P
   ;


WORK
   : W O R K
   ;


YEAR
   : Y E A R
   ;


YELLOW
   : Y E L L O W
   ;

//----------------------------------------------------------------------------
// OPERATORS
//----------------------------------------------------------------------------

PLUS
   : '+'
   ;


MINUS
   : '-'
   ;


STAR
   : '*'
   ;


SLASH
   : '/'
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


EQUAL
   : '='
   ;

//NOT_EQUAL       : "<>" | "!="  ;
// Why did I do this?  Isn't this handle by just increasing the look ahead?

NOT_EQUAL
   : '<' (('>') | ('='))? | '!=' | '^='
   ;


LT
   : '<'
   ;


LE
   : '<='
   ;


GE
   : '>='
   ;


GT
   : '>'
   ;

//GT
//   : '>' ('=')?
//   ;

LPAREN
   : '('
   ;


RPAREN
   : ')'
   ;


LBRACK
   : '['
   ;


RBRACK
   : ']'
   ;


DOT
   : '.'
   ;


ATSYMBOL
   : '@'
   ;


DOUBLEVERTBAR
   : '||'
   ;

//COMMENT_1
//      :  '{'
//	    ( 
//        :   '\r' '\n'       
//	    |	'\r'			
//	    |	'\n'			
//   |   ~('}' | '\n' | '\r')
//		    )*
//         '}'
//	;
// an identifier.  Note that testLiterals is set to true!  This means
// that after we match the rule, we look in the literals table to see
// if it's a literal or really an identifer

IDENT
   : ('a' .. 'z' | '_') ('a' .. 'z' | '_' | '0' .. '9')*
   ;

// character literals
/*
CHAR_LITERAL
	:	'\'' ( ESC | ~'\'' ) '\''
	;
	*/
// string literals

STRING_LITERAL
   : ('"') (
   /*ESC*|*/ ~ ('"' | '\\'))* ('"') | ('\'') (
   /*ESC|~*/ ('\\' | '\''))* ('\'')
   ;

// a numeric literal

NUM_INT
   : '.' (('0' .. '9')+)? | (('0' .. '9') ('0' .. '9')*)
   ;


NUM_REAL
   : ('0' .. '9')+ '.' ('0' .. '9')* EXPONENT? | '.' ('0' .. '9')+ EXPONENT? | ('0' .. '9')+ EXPONENT
   ;


fragment EXPONENT
   : ('e' | 'E') ('+' | '-')? ('0' .. '9')+
   ;

// escape sequence -- note that this is protected; it can only be called
//   from another lexer rule -- it will not ever directly return a token to
//   the parser
// There are various ambiguities hushed in this rule.  The optional
// '0'...'9' digit matches should be matched here rather than letting
// them go back to STRING_LITERAL to be matched.  ANTLR does the
// right thing by matching immediately; hence, it's ok to shut off
// the FOLLOW ambig warnings.
/*
ESC
	:	'\\'
		(	'n'
		|	'r'
		|	't'
		|	'b'
		|	'f'
		|	'"'
		|	'\''
		|	'\\'
		|	('u')+ HEX_DIGIT HEX_DIGIT HEX_DIGIT HEX_DIGIT
		|	'0'..'3'
			(
				
			:	'0'..'7'
				(
					
				:	'0'..'7'
				)?
			)?
		|	'4'..'7'
			(
				
			:	'0'..'7'
			)?
		)
	;
*/
// hexadecimal digit (again, note it's protected!)

HEX_DIGIT
   : ('0' .. '9' | 'a' .. 'f')
   ;

// Single-line comments

SL_COMMENT
   : '#' ~ [\r\n]* -> skip
   ;


SL_COMMENT_2
   : '--' ~ [\r\n]* -> skip
   ;


EOL
   : [\r\n] +
   ;


WS
   : [ \t] + -> skip
   ;
