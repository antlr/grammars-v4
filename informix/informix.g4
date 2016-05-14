grammar informix;


compilation_unit    
    : 
     ( databaseDeclaration )?
     ( globalDeclaration   )?
       typeDeclarations    
     ( mainBlock           )?
     functionOrReportDefinitions
     EOF
    ;

identifier
    : IDENT
    ;

    
mainStatements
    :
        (        
         deferStatement 
        | 
         codeBlock 
        )*
    ;
mainBlock
    : 
      MAIN eol
        typeDeclarations
        mainStatements                      
      END MAIN eol
    ;

deferStatement
    :
      DEFER (INTERRUPT | QUIT) eol
    ;

functionOrReportDefinitions
    :
      (reportDefinition | functionDefinition)*
    ;


returnStatement
    : RETURN (variableOrConstantList)?
    ;
    
functionDefinition
    : FUNCTION functionIdentifier  parameterList  eol
        typeDeclarations
        (codeBlock)?
      END FUNCTION eol
    ;

parameterList
    : empty
    | LPAREN ( parameterGroup )* RPAREN
    ;

parameterGroup
    : identifier ( COMMA identifier )* 
    ;

globalDeclaration
    :
     GLOBALS 
      (
         string
       | eol typeDeclarations
         END GLOBALS 
      ) eol
    ;
    
typeDeclarations 
    : (typeDeclaration)* 
    ;

typeDeclaration
    : 
      DEFINE variableDeclaration (COMMA variableDeclaration)* 
    ;


variableDeclaration
    :       
      constantIdentifier ( COMMA constantIdentifier )* type       
      |
      constantIdentifier type ( COMMA constantIdentifier type )*      
    ;

type
    : typeIdentifier
    | indirectType
    | largeType
    | structuredType
    ;

indirectType
    : LIKE    tableIdentifier DOT identifier
    ;
    
typeIdentifier
    : charType
    | numberType
    | timeType
    ;

largeType
    :
      TEXT
    | BYTE
    ;

numberType
    :
      BIGINT
    | INTEGER
    | INT
    | SMALLINT
    | REAL
    | SMALLFLOAT
    | (
       DECIMAL |
       DEC     |
       NUMERIC |
       MONEY
      )
      (
       LPAREN numericConstant (COMMA numericConstant )? RPAREN
      |
      )
    | (
       FLOAT |
       DOUBLE
      )
      (
       LPAREN numericConstant RPAREN
      |
      )
    ;

charType
    :
      (
       VARCHAR |
       NVARCHAR 
      )      
       LPAREN numericConstant (COMMA numericConstant )? RPAREN
    | (
       CHAR |
       NCHAR |
       CHARACTER
      )
      (
       LPAREN numericConstant RPAREN
      |
      )
    ;

timeType
    : DATE
    | DATETIME datetimeQualifier
    | INTERVAL intervalQualifier
    ;
    
datetimeQualifier
    : YEAR     TO yearQualifier
    | MONTH    TO monthQualifier
    | DAY      TO dayQualifier
    | HOUR     TO hourQualifier
    | MINUTE   TO minuteQualifier
    | SECOND   TO secondQualifier
    | FRACTION TO fractionQualifier        
    ;

intervalQualifier
    : YEAR     ( LPAREN numericConstant RPAREN )? TO yearQualifier
    | MONTH    ( LPAREN numericConstant RPAREN )? TO monthQualifier
    | DAY      ( LPAREN numericConstant RPAREN )? TO dayQualifier
    | HOUR     ( LPAREN numericConstant RPAREN )? TO hourQualifier
    | MINUTE   ( LPAREN numericConstant RPAREN )? TO minuteQualifier
    | SECOND   ( LPAREN numericConstant RPAREN )? TO secondQualifier
    | FRACTION  TO fractionQualifier        
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
    : FRACTION ( LPAREN numericConstant RPAREN )? 
    ;

    
structuredType
    : recordType
    | arrayType
    | dynArrayType
    ;

recordType
    : RECORD 
      ( eol ( variableDeclaration (COMMA variableDeclaration)* )
        END RECORD
       |  
        ( LIKE tableIdentifier DOT STAR )
      ) 
      
    ;


arrayIndexer
    : LBRACK numericConstant (COMMA numericConstant |  COMMA numericConstant COMMA numericConstant)? RBRACK
    ;
    
arrayType
    : ARRAY arrayIndexer OF
      ( recordType
      | typeIdentifier
      | largeType)
    ;
    
dynArrayType
    : DYNAMIC ARRAY WITH numericConstant DIMENSIONS OF   
      ( recordType
      | typeIdentifier
      )    
    ;    

string
    : STRING_LITERAL
    ;

statement
    : ( label COLON ) ?
      unlabelledStatement
    ;

codeBlock
    : (

	:
	statement
	|
	databaseDeclaration
       )+
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
    | sqlStatements  (SEMI)?
    | otherFGLStatement
    | menuInsideStatement
    | constructInsideStatement
    | displayInsideStatement
    | inputInsideStatement
    ;

runStatement
    : RUN (variable | string) 
     (IN FORM MODE  | IN LINE MODE )?
     (WITHOUT WAITING | RETURNING variable )?
    ;
    
assignmentStatement
    : LET variable EQUAL expression ( COMMA  expression )*
    ;

procedureStatement
    : CALL procedureIdentifier
        ( LPAREN (actualParameter ( COMMA actualParameter )*)? RPAREN
        | empty
        )
        (
         RETURNING variable ( COMMA variable )* 
        )?
    ;

procedureIdentifier
    : functionIdentifier
    ;

actualParameter
    : STAR
    | expression
    ;

gotoStatement
    : GOTO ( COLON )? label eol
    ;

/*
emptyStatement
    : empty eol!
    ;
*/

empty
    : /* empty */
    ;
    
condition 
    : TRUE
      |
      FALSE
      |
      logicalTerm  ( OR logicalTerm  )* 
    ;

logicalTerm 
    : logicalFactor ( AND logicalFactor )* 
    ;


logicalFactor 
      :
      // Added "prior" to a comparison expression to support use of a
      // condition in a connect_clause.
        ( sqlExpression  ( NOT )? IN )  sqlExpression  (NOT)? IN expressionSet
      | ( sqlExpression  ( NOT )? LIKE )  sqlExpression  ( NOT )? LIKE sqlExpression ( ESC
QUOTED_STRING )?
      | ( sqlExpression  ( NOT )? BETWEEN )  sqlExpression  ( NOT )? BETWEEN sqlExpression  AND
sqlExpression 
      | ( sqlExpression  IS ( NOT )? NULL )  sqlExpression  IS ( NOT )? NULL
      | ( quantifiedFactor )  quantifiedFactor 
      | ( NOT condition )  NOT condition
      | ( LPAREN condition RPAREN ) 
      | sqlExpression  relationalOperator sqlExpression 
    ;


quantifiedFactor 
      :
      ( sqlExpression  relationalOperator ( ALL | ANY )? subquery )  sqlExpression  relationalOperator ( ALL | ANY )? subquery
      | ( ( NOT )? EXISTS subquery )  ( NOT )? EXISTS subquery
      | subquery
    ;

expressionSet 
      : ( sqlExpression  )  sqlExpression 
      | subquery
    ;

subquery 
    :
        LPAREN sqlSelectStatement RPAREN         
    ;


sqlExpression 
      : sqlTerm ( ( PLUS | MINUS ) sqlTerm )*
    ;

sqlAlias 
      : ( AS )? identifier
    ;

sqlTerm 
      : sqlFactor ( ( sqlMultiply | DIV | SLASH ) sqlFactor )*
    ;

sqlMultiply:
        STAR
    ;

sqlFactor
      : sqlFactor2 ( DOUBLEVERTBAR sqlFactor2 )*
    ;

sqlFactor2
      : ( sqlVariable (UNITS unitType)?)  sqlVariable (UNITS unitType)?      
      | ( sqlLiteral (UNITS unitType)?)  sqlLiteral (UNITS unitType)?
      | ( groupFunction LPAREN (STAR | ALL | DISTINCT )? (sqlExpression  ( COMMA sqlExpression  )* )? RPAREN )
          groupFunction LPAREN (STAR | ALL | DISTINCT )? (sqlExpression  ( COMMA sqlExpression  )* )? RPAREN 
      | ( sqlFunction ( LPAREN sqlExpression  ( COMMA sqlExpression  )* RPAREN ) ) 
          sqlFunction ( LPAREN sqlExpression  ( COMMA sqlExpression  )* RPAREN )
      | ( ( PLUS | MINUS ) sqlExpression  )  ( PLUS | MINUS ) sqlExpression 
      | ( LPAREN sqlExpression  RPAREN )  LPAREN sqlExpression  RPAREN
      | sqlExpressionList
    ;

sqlExpressionList : LPAREN sqlExpression ( COMMA sqlExpression )+ RPAREN ;

sqlLiteral:
        ( unsignedConstant | string | NULL  | FALSE | TRUE )
    ;

sqlVariable 
    :
      (columnsTableId) columnsTableId  // ( LPAREN PLUS RPAREN )?  columnsTableId ( LPAREN PLUS RPAREN )?
    ;

sqlFunction 
      :
      numberFunction 
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
      :
      MOD 
    ;


charFunction 
      :
      LENGTH
    ;

groupFunction 
      :
      AVG | COUNT | MAX | MIN | SUM
    ;
    
otherFunction 
      :
      DECODE  | NVL  | constantIdentifier
    ;

// This is not being used currently, but might be useful at some point.
sqlPseudoColumn
    :
        USER 
    ;


relationalOperator
    : EQUAL | NOT_EQUAL | LE | LT | GE | GT | (NOT)? MATCHES | LIKE 
    ;


ifCondition 
    : TRUE
      |
      FALSE
      |
      ifCondition2 (relationalOperator ifCondition2)*    
    ;
      
    
ifCondition2
    : ifLogicalTerm  ( OR ifLogicalTerm  )* 
    ;

ifLogicalTerm 
    : ifLogicalFactor ( AND ifLogicalFactor )* 
    ;

    
expression
    : simpleExpression (CLIPPED | USING string )*  
    ;

ifLogicalFactor 
      :
      // Added "prior" to a comparison expression to support use of a
      // condition in a connect_clause.
        (simpleExpression  IS (NOT)? NULL)   simpleExpression IS (NOT)? NULL
      | ( NOT ifCondition )  NOT ifCondition
      | LPAREN ifCondition RPAREN
      | (simpleExpression)  simpleExpression      
      ;

simpleExpression
    : ( sign )?
      term ( addingOperator term )*
    ;

addingOperator
    : PLUS | MINUS 
    ;

term
    : factor ( multiplyingOperator factor )*
    ;

multiplyingOperator
    : STAR | SLASH | DIV | MOD 
    ;

factor
    : ((GROUP)? functionDesignator 
    | variable 
    | constant
    | LPAREN expression RPAREN
    | NOT factor    ) (UNITS unitType)?
    ;


functionDesignator
    : functionIdentifier
        ( LPAREN (actualParameter ( COMMA actualParameter )*)? RPAREN )?
    ;

functionIdentifier
    : DAY
    | YEAR
    | MONTH
    | TODAY
    | WEEKDAY
    | MDY
    | COLUMN
    | SUM
    | COUNT
    | AVG
    | MIN
    | MAX
    | EXTEND
    | DATE
    | TIME
    | INFIELD
    | PREPARE
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
    :  LBRACK expression ( COMMA expression)* RBRACK
    ;
    /*
thruNotation
    :
     ( (THROUGH |THRU) (SAME DOT)? identifier )?
    ;
*/
componentVariable
    : (recordVariable (indexingVariable)?) ((DOT STAR)| (DOT componentVariable ((THROUGH |THRU) componentVariable )? ) )?
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
    : IF ifCondition THEN 
        (codeBlock )?
      ( ELSE (codeBlock )? )?
      END IF 
    ;

repetetiveStatement
    : whileStatement
    | forEachStatement
    | forStatement
    ;

whileStatement
    : WHILE ifCondition 
       (codeBlock )?
      END WHILE  
    ;

forStatement
    : FOR controlVariable EQUAL forList 
        ( STEP numericConstant )?  eol
        (codeBlock )?
      END FOR  eol
    ;

forList
    : initialValue TO  finalValue
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
    : FOREACH identifier
        (USING variableList )?
        (INTO variableList )?
        (WITH REOPTIMIZATION )?  eol
        (codeBlock )?
      END FOREACH  eol
    ;

variableList
    : variable (COMMA variable)*
    ;

variableOrConstantList
    : (expression) (COMMA (expression ))*
    ;

caseStatement
    : CASE expression   
        ( WHEN expression (codeBlock)? )*
        ( OTHERWISE (codeBlock)? )?
      END CASE 
      |
      CASE 
        ( WHEN ifCondition codeBlock )*
        ( OTHERWISE codeBlock )?
      END CASE       
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
    | EXIT PROGRAM ( LPAREN expression RPAREN | expression )?  
    ;

continueStatements
    : CONTINUE exitTypes eol
    ;
        
otherStorageStatement
    : ALLOCATE ARRAY identifier arrayIndexer 
    | LOCATE variableList IN ( MEMORY | FILE (variable | string)? ) 
    | DEALLOCATE ARRAY identifier 
    | RESIZE ARRAY identifier arrayIndexer 
    | FREE variable (COMMA variable)* 
    | INITIALIZE variable (COMMA variable)* (TO NULL | LIKE expression (COMMA expression)* ) 
    | VALIDATE variable (COMMA variable)* LIKE expression (COMMA expression)*
    ;

printExpressionItem
    : COLUMN expression
    | PAGENO
    | LINENO
    | BYTE variable
    | TEXT variable
    | expression  (SPACE | SPACES)? (WORDWRAP (RIGHT MARGIN numericConstant)?)?
    ;
    
printExpressionList
    : printExpressionItem (COMMA printExpressionItem)*
    ;
    
reportStatement
    : START REPORT constantIdentifier
          (TO 
           (
            expression
            |
            PIPE expression
            |
            PRINTER
           )
          )? 
          (WITH  
           (
	       (LEFT   MARGIN numericConstant )
	     | (RIGHT  MARGIN numericConstant )
	     | (TOP    MARGIN numericConstant )
	     | (BOTTOM MARGIN numericConstant )        
	     | (PAGE   LENGTH numericConstant )
	     | (TOP OF PAGE string )
           )*
          )? 
    | TERMINATE REPORT constantIdentifier
    | FINISH REPORT constantIdentifier
    | PAUSE (string)? 
    | NEED expression LINES 
    | PRINT ((printExpressionList)? (SEMI)? | FILE string)?
    | SKIP ( expression (LINE | LINES) | TO TOP OF PAGE  )
    | OUTPUT TO REPORT constantIdentifier  LPAREN (expression ( COMMA expression )*)? RPAREN 
    ;

thruNotation
    :
     ( (THROUGH |THRU) (SAME DOT)? identifier )?
    ;
    
    
fieldName
    :
       ( (identifier (LBRACK numericConstant RBRACK)? )  DOT )? identifier 
     | (identifier (LBRACK numericConstant RBRACK)? )  DOT  (STAR   | identifier thruNotation  )
    ;

fieldList
    : expression (COMMA expression)*
    ;

    
keyList
    :
     expression (COMMA expression)*
    ;   
     
constructEvents
    : BEFORE CONSTRUCT
    | AFTER CONSTRUCT
    | BEFORE FIELD fieldList
    | AFTER FIELD fieldList
    | ON KEY LPAREN (keyList) RPAREN
    ;
    
constructInsideStatement    
    : NEXT FIELD  
       (  fieldName          
        | NEXT
        | PREVIOUS
       )  eol
    | CONTINUE CONSTRUCT  eol
    | EXIT CONSTRUCT  eol
    ;

specialAttribute
    : REVERSE
    | BLINK 
    | UNDERLINE
    ;
    
attribute
    : (  BLACK
      | BLUE
      | CYAN
      | GREEN
      | MAGENTA
      | RED
      | WHITE
      | YELLOW
      | BOLD
      | DIM
      | NORMAL
      | INVISIBLE ) ?
      specialAttribute (COMMA specialAttribute)*      
    ;

attributeList
    : (ATTRIBUTE | ATTRIBUTES)  LPAREN attribute RPAREN
    ;    
    
constructGroupStatement  
    :
    constructEvents
    (codeBlock)+
    ;
    
constructStatement
    :CONSTRUCT 
      (
         BY NAME variable ON columnsList
       | variable ON columnsList
         FROM fieldList
      )
      ( attributeList )?
      ( HELP numericConstant )?
      (
       (constructGroupStatement)+
       END CONSTRUCT
      )?
    ;

displayArrayStatement
    : DISPLAY ARRAY expression TO expression
       ( attributeList )?
       ( displayEvents )*
       ( END DISPLAY   )?             
    ;


displayInsideStatement    
    : CONTINUE DISPLAY
    | EXIT DISPLAY
    ;


displayEvents
    : 
      ON KEY LPAREN keyList RPAREN 
       (
        (codeBlock)+
       )          
    ;
    
    
displayStatement
    : DISPLAY
       (  BY NAME ( expression (COMMA expression)* )
        | ( expression (COMMA expression)* )
         (
          TO fieldList
          |
          AT expression COMMA expression
         )?
       )
       ( attributeList )?
      eol
    ;
    
errorStatement
    : ERROR expression (COMMA expression)* (attributeList)? 
    ;

messageStatement
    : MESSAGE expression (COMMA expression)* (attributeList)? 
    ;
    
promptStatement
    : PROMPT expression (COMMA expression)* (attributeList)? 
       FOR (CHAR)? variable
      ( HELP numericConstant )?
      ( attributeList )?
      (
        (ON KEY LPAREN (keyList) RPAREN
           (codeBlock)?
        ) *
        END PROMPT
      )?
       
    ;

inputEvents
    : (BEFORE | AFTER ) (INPUT | ROW | INSERT | DELETE )
    | BEFORE FIELD fieldList
    | AFTER FIELD fieldList
    | ON KEY LPAREN keyList RPAREN
    ;
    
inputInsideStatement    
    : NEXT FIELD  
       (  fieldName          
        | NEXT
        | PREVIOUS
       )  
    | CONTINUE INPUT
    | EXIT INPUT
    ;

inputGroupStatement  
    :
     inputEvents
     (codeBlock)*
    ;

inputStatement
    : INPUT
       ( BY NAME expression (COMMA expression)*
           (WITHOUT DEFAULTS)?
       |
         expression (COMMA expression)*
           (WITHOUT DEFAULTS)? 
           FROM
            fieldList 
       )
      ( attributeList )?
      ( HELP numericConstant )?
      (
       (inputGroupStatement)+
       END INPUT
      )?       
    ;
    
inputArrayStatement
    : INPUT ARRAY expression
       (WITHOUT DEFAULTS)? 
           FROM expression (COMMA expression)*
      ( HELP numericConstant )?
      ( attributeList )?
      (
       (inputGroupStatement)+
       END INPUT
      )?       
    ;

menuEvents
    : BEFORE MENU
    | COMMAND 
       ( (KEY LPAREN keyList RPAREN )? 
          expression
         ( expression )?
         ( HELP numericConstant )?
       )
    ;


menuInsideStatement    
    : NEXT OPTION  (expression | ALL) (COMMA expression)*
    | SHOW OPTION  (expression | ALL) (COMMA expression)*
    | HIDE OPTION  (expression | ALL) (COMMA expression)*
    | CONTINUE MENU
    | EXIT MENU
    ;

    
menuGroupStatement    
    : menuEvents (codeBlock)? 
    ;
    
menuStatement
    : MENU expression
        (menuGroupStatement)*
      END MENU    
    ;    

reservedLinePosition 
    : FIRST (PLUS numericConstant)?
    | numericConstant
    | LAST (MINUS numericConstant)?
    ;
    
specialWindowAttribute
    : (  BLACK
      | BLUE
      | CYAN
      | GREEN
      | MAGENTA
      | RED
      | WHITE
      | YELLOW
      | BOLD
      | DIM
      | NORMAL
      | INVISIBLE )
    | REVERSE
    | BORDER
    | ( PROMPT
      | FORM 
      | MENU
      | MESSAGE 
      ) LINE (reservedLinePosition)
    | COMMENT LINE (reservedLinePosition | OFF)
    ;
    
windowAttribute
    :  specialWindowAttribute (COMMA specialWindowAttribute)*      
    ;

windowAttributeList
    : (ATTRIBUTE | ATTRIBUTES) LPAREN windowAttribute RPAREN
    ;  

optionStatement
    :  (
          MESSAGE LINE expression
        | 
          PROMPT  LINE expression
        |
          MENU    LINE expression
        | 
          COMMENT LINE expression
        | 
          ERROR   LINE expression
        |
          FORM    LINE expression
        |
         INPUT   (WRAP | NO WRAP)
        |
         INSERT   KEY expression
        |
         DELETE   KEY expression
        |
         NEXT     KEY expression
        |
         PREVIOUS KEY expression
        |
         ACCEPT   KEY expression
        |
         HELP     FILE expression
        | 
         HELP     KEY expression
        |
         INPUT attributeList
        |
         DISPLAY  attributeList
        |
         SQL INTERRUPT (ON | OFF) 
        |
         FIELD ORDER 
          (
            CONSTRAINED
           |
            UNCONSTRAINED
          )
       )
    ;

optionsStatement          
: OPTIONS 
       optionStatement (COMMA optionStatement)*
;

screenStatement 
    : CLEAR 
       ( FORM 
       | WINDOW identifier
       | (WINDOW)? SCREEN  
       | fieldList
       ) 
    | CLOSE WINDOW identifier eol
    | CLOSE FORM identifier eol
    | constructStatement
    | CURRENT WINDOW IS (SCREEN | identifier) eol
    | displayStatement
    | displayArrayStatement
    | DISPLAY FORM identifier (attributeList)? eol
    | errorStatement 
    | messageStatement
    | promptStatement
    | inputStatement
    | inputArrayStatement
    | menuStatement
    | OPEN FORM expression FROM expression
    | OPEN WINDOW expression
        AT expression COMMA  expression
        ( WITH FORM  expression  
         |
         WITH expression ROWS COMMA  expression COLUMNS
        )
        (windowAttributeList)?
    | optionsStatement
    | SCROLL fieldList (COMMA fieldList)*
       (UP | DOWN) (BY numericConstant)?
    ;
    
sqlStatements
    : cursorManipulationStatement
    | dataDefinitionStatement
    | dataManipulationStatement
    | dynamicManagementStatement
    | queryOptimizationStatement
    //| dataAccessStatement
    | dataIntegrityStatement
    //| storedProcedureStatement
    | clientServerStatement
    //| opticalStatement
    ;

    
cursorManipulationStatement
    : CLOSE cursorName eol
    | DECLARE cursorName 
      (
        CURSOR (WITH HOLD)? FOR
         (
           sqlSelectStatement
            (FOR UPDATE (OF columnsList)? )?
           |
           sqlInsertStatement
           |
           statementId 
         )
        |
        SCROLL CURSOR (WITH HOLD)? FOR 
          ( sqlSelectStatement | statementId )
      )
    | FETCH 
       (
          NEXT
        | (PREVIOUS |  PRIOR)
        | FIRST
        | LAST
        | CURRENT
        | RELATIVE expression
        | ABSOLUTE expression
       )?
       cursorName 
       ( INTO variableList )?
    | FLUSH cursorName eol
    | OPEN cursorName  ( USING variableList )?
    | PUT cursorName   ( FROM variableOrConstantList )?
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
    : constantIdentifier
      (
        dataType 
       |
        (BYTE | TEXT  ) 
         ( IN (TABLE | constantIdentifier ) )?
      )
      (NOT NULL)?         
     | 
       UNIQUE LPAREN (constantIdentifier (COMMA constantIdentifier)*)? RPAREN
         (CONSTRAINT constantIdentifier )?
      
    ;    
    
dataDefinitionStatement
    : DROP TABLE constantIdentifier 
     |
       CREATE (TEMP)? TABLE constantIdentifier 
       LPAREN 
         columnItem (COMMA columnItem) *
       RPAREN
       (WITH NO LOG )?
       (IN constantIdentifier )?
       (EXTENT SIZE numericConstant )?
       (NEXT SIZE numericConstant )?
       (LOCK MODE LPAREN ( PAGE | ROW ) RPAREN)?
     |
       CREATE (UNIQUE)? (CLUSTER)?
        INDEX constantIdentifier 
          ON  constantIdentifier 
          LPAREN constantIdentifier (ASC |DESC)? (COMMA constantIdentifier (ASC |DESC)?)* RPAREN
     |
       DROP INDEX constantIdentifier      
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
    : 
      mainSelectStatement
    ;


/*
columnsTableId 
    : (tableIdentifier DOT)? (STAR | identifier (LBRACK expression ( COMMA expression)* RBRACK)?)
    ;
  */  
  
columnsTableId 
    : STAR 
    | ( tableIdentifier (indexingVariable)?) ( DOT STAR | DOT columnsTableId )? 
    ;
    
    
selectList
    : ( (sqlExpression ) (sqlAlias)? 
       (COMMA 
        (sqlExpression ) (sqlAlias)? 
       )* )       
    ;

headSelectStatement
    : SELECT (ALL | (DISTINCT | UNIQUE))? selectList 
    ;
    
tableQualifier
    :
      ( 
        ( constantIdentifier COLON
         |
          constantIdentifier ATSYMBOL constantIdentifier COLON
         )?
       |
        ( string)?        
      )?
    ;

tableIdentifier
    :
      tableQualifier constantIdentifier
    ;
    
fromTable
    : (OUTER)? tableIdentifier (sqlAlias)?
    ;

tableExpression
    :
      simpleSelectStatement     
    ;
    
fromSelectStatement
    : FROM 
      ( fromTable
      | LPAREN tableExpression  RPAREN (sqlAlias)?
      )
      ( COMMA
        ( fromTable
        | LPAREN tableExpression   RPAREN (sqlAlias)?
        )
      )*      
    ;

aliasName
    : identifier
    ;
    
mainSelectStatement
    : headSelectStatement
      (INTO variableList)?
      fromSelectStatement
      (whereStatement)?
      (groupByStatement )?
      (havingStatement)?
      (unionSelectStatement)?
      (orderbyStatement)?
      (INTO TEMP identifier)?
      (WITH NO LOG)?
    ;

unionSelectStatement
    : ( UNION (ALL)? simpleSelectStatement)
    ;
        
simpleSelectStatement
    : headSelectStatement
      fromSelectStatement
      (whereStatement)?
      (groupByStatement )?
      (havingStatement)?     
      (unionSelectStatement)? 
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
    : expression  (ASC | DESC)?
    ;        
    
orderbyStatement
    : ORDER BY  orderbyColumn (COMMA orderbyColumn)*
    ;


sqlLoadStatement
    : 
     LOAD FROM (variable | string) 
      (DELIMITER (variable | string) )?
      (
       INSERT INTO tableIdentifier 
        ( LPAREN columnsList RPAREN )?
      |
       sqlInsertStatement 
      ) eol
    ;  
    

sqlUnLoadStatement
    : 
     UNLOAD TO (variable | string) 
      (DELIMITER (variable | string) )?      
       sqlSelectStatement   eol
    ;  
    
sqlInsertStatement
    : INSERT INTO tableIdentifier 
       ( LPAREN columnsList RPAREN )?
       ( 
        VALUES 
           LPAREN 
            expression (COMMA expression)*
           RPAREN
       |
        simpleSelectStatement        
       )
    ;  
    
sqlUpdateStatement
    : UPDATE tableIdentifier SET
      (
        ( 
         columnsTableId EQUAL expression
         (COMMA columnsTableId EQUAL expression)*
         )
       |
        (
         ( LPAREN columnsList RPAREN 
          | (aliasName DOT )? STAR
         )
         EQUAL 
         (
           LPAREN 
            expression (COMMA expression)*
           RPAREN
          | 
           (aliasName DOT )? STAR
         )
        )
      )
      (
       WHERE 
        ( 
         condition
         |
         CURRENT OF cursorName
        )
      )?      
    ;   

sqlDeleteStatement
    : DELETE FROM tableIdentifier 
      (
       WHERE 
        ( 
         condition
         |
         CURRENT OF cursorName
        )
      )?  eol
    ;   
    
         
actualParameterList
    : actualParameter (COMMA actualParameter)*
    ;
    
dynamicManagementStatement
    : PREPARE cursorName FROM expression
    | EXECUTE cursorName  ( USING variableList )? 
    | FREE (cursorName | statementId)
    | LOCK TABLE expression IN (SHARE | EXCLUSIVE) MODE
    ;
    
queryOptimizationStatement
    : UPDATE STATISTICS (FOR TABLE tableIdentifier)?
    | SET LOCK MODE TO (WAIT (SECONDS)? |  NOT WAIT )
    | SET EXPLAIN (ON | OFF)
    | SET ISOLATION TO 
       ( CURSOR STABILITY
       | (DIRTY | COMMITTED | REPEATABLE )
         READ
       )
    | SET (BUFFERED)? LOG
    ;
    
databaseDeclaration
    : DATABASE 
        ( 
          constantIdentifier (ATSYMBOL constantIdentifier)?
        )
      (EXCLUSIVE)? (SEMI)?
    ;    
    
clientServerStatement
    :  CLOSE DATABASE
    ;
    
    
dataIntegrityStatement
    : wheneverStatement
    | BEGIN WORK 
    | COMMIT WORK 
    | ROLLBACK WORK 
    ;

wheneverStatement
    : WHENEVER  wheneverType wheneverFlow eol
    ;
   
wheneverType
     :
       NOT FOUND 
     | (ANY)?
       ( SQLERROR 
       | ERROR
       ) 
     | ( SQLWARNING 
       | WARNING 
       )
     ;
    
wheneverFlow
     : CONTINUE
     | STOP
     | CALL identifier 
     | (GO TO | GOTO ) (COLON)? identifier 
     ;

reportDefinition
    : REPORT identifier  parameterList  
	(typeDeclarations )?
        (outputReport)?
        (ORDER (EXTERNAL)? BY variableList )?
        (formatReport)?
      END REPORT 
    ;
    
outputReport
    : OUTPUT 
        (
         REPORT TO 
           (
            string 
            |
            PIPE string 
            |
            PRINTER
           )
        )?
        (
           (LEFT   MARGIN numericConstant )
         | (RIGHT  MARGIN numericConstant )
         | (TOP    MARGIN numericConstant )
         | (BOTTOM MARGIN numericConstant )        
         | (PAGE   LENGTH numericConstant )
         | (TOP OF PAGE string )
        )*                
    ;
    
formatReport
    : FORMAT 
        (
          EVERY ROW 
         |  
          (
            (
              (FIRST)? PAGE HEADER
              |
              PAGE TRAILER
              |
              ON (EVERY ROW | LAST ROW) 
              |
              (BEFORE | AFTER) GROUP OF variable
            )
            codeBlock
          )+
        )
    ;    

eol
	: /*
	(
	options {
			warnWhenFollowAmbig = false;
		}
		:
		EOL!
	)+ */
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
    : PLUS | MINUS
    ;

constantIdentifier
    : ACCEPT
    | ASCII
    | COUNT
    | CURRENT
    | FALSE
    | FIRST
    | FOUND
    | GROUP
    | HIDE
    | INDEX 
    | INT_FLAG
    | INTERRUPT
    | LAST
    | LENGTH
    | LINENO
    | MDY
    | NO
    | NOT
    | NOTFOUND
    | NULL
    | PAGENO
    | REAL
    | SIZE
    | SQL
    | STATUS
    | TEMP
    | TIME
    | TODAY
    | TRUE
    | USER
    | WAIT
    | WEEKDAY
    | WORK
    | identifier
    ;




  ABSOLUTE         : 'absolute'        ;
  AFTER            : 'after'           ;
  ACCEPT           : 'accept'          ;
  AGGREAGATE       : 'aggregate'       ;
  ALLOCATE         : 'allocate'        ;
  ALL              : 'all'             ;
  ALL_ROWS         : 'all_rows'        ;
  AND              : 'and'             ;
  ANY              : 'any'             ;
  AS               : 'as'              ;
  ASC              : 'asc'             ;  
  ASCII            : 'ascii'           ;      
  AT               : 'at'              ;
  ATTRIBUTE        : 'attribute'       ;
  ATTRIBUTES       : 'attributes'      ;
  AVERAGE          : 'average'         ;
  AVG              : 'avg'             ;  
  ARRAY            : 'array'           ;
  BEFORE           : 'before'          ; 
  BEGIN            : 'begin'           ; //TRANSACTION CONTROL
  BETWEEN          : 'between'         ;      
  BIGINT           : 'bigint'          ;
  BLACK		   : 'black'           ;
  BLINK            : 'blink'           ;
  BLUE 		   : 'blue'            ;
  BOLD             : 'bold'            ;
  BORDER           : 'border'          ;
  BOTTOM           : 'bottom'          ;
  BUFFERED         : 'buffered'        ;
  BY               : 'by'              ;
  BYTE             : 'byte'            ;
  CACHE 	   : 'cache'           ;
  CALL             : 'call'            ;
  CASE             : 'case'            ;
  CHAR             : 'char'            ;
  CHARARACTER      : 'character'       ;
  CHAR_LENGTH      : 'char_length'     ;
  CHECK 	   : 'check'           ;
  CLEAR 	   : 'clear'           ;
  CLIPPED          : 'clipped'         ;      
  CLOSE 	   : 'close'           ;
  CLUSTER 	   : 'cluster'         ;
  COLUMN           : 'column'          ;      
  COLUMNS          : 'columns'         ;      
  COMMAND          : 'command'         ;
  COMMENT          : 'comment'         ;
  COMMIT           : 'commit'          ;
  COMMITED         : 'commited'        ;
  CONSTANT         : 'constant'        ;
  CONSTRAINED      : 'constrained'     ;  
  CONSTRAINT       : 'constraint'      ;  
  CONSTRUCT        : 'construct'       ;
  CONTINUE         : 'continue'        ;
  COUNT            : 'count'           ;
  COPY             : 'copy'            ;
  CRCOLS           : 'crcols'          ;
  CREATE           : 'create'          ;
  CURRENT          : 'current'         ;
  CURSOR	   : 'cursor'          ;
  CYAN 		   : 'cyan'            ;
  DATABASE         : 'database'        ;
  DATE             : 'date'            ;
  DATETIME         : 'datetime'        ;
  DAY              : 'day'             ;      
  DEALLOCATE       : 'deallocate'      ;
  DEC              : 'dec'             ;
  DECIMAL          : 'decimal'         ;
  DECODE           : 'decode'          ;
  DECLARE          : 'declare'         ;
  DEFAULT          : 'default'         ;
  DEFAULTS         : 'defaults'        ;
  DEFER            : 'defer'           ;
  DEFINE           : 'define'          ;
  DELETE           : 'delete'          ; //SQL
  DELIMITER        : 'delimiter'       ; //SQL
  DESC             : 'desc'            ;
  DIM              : 'dim'             ;      
  DIMENSIONS       : 'dimensions'      ;
  DIRTY            : 'dirty'           ;
  DISPLAY          : 'display'         ;
  DISTINCT         : 'distinct'        ;
  DO               : 'do'              ;
  DOUBLE           : 'double'          ;
  DOWN             : 'down'            ;
  DROP             : 'drop'            ;
  DYNAMIC          : 'dymanic'         ;
  ELSE             : 'else'            ;
  END              : 'end'             ;
  ERROR            : 'error'           ;
  ESCAPE           : 'escape'          ;      
  EVERY            : 'every'           ;      
  EXCLUSIVE        : 'exclusive'       ;
  EXEC             : 'exec'            ;
  EXECUTE          : 'execute'         ;
  EXIT             : 'exit'            ;
  EXISTS           : 'exists'          ;
  EXPLAIN          : 'explain'         ;
  EXTEND           : 'extend'          ;      
  EXTENT           : 'extent'          ;      
  EXTERNAL         : 'external'        ;
  FALSE 	   : 'false'           ; 
  FETCH            : 'fetch'           ;
  FIELD            : 'field'           ;      
  FIELD_TOUCHED    : 'field_touched'   ;      
  FILE             : 'file'            ;
  FINISH           : 'finish'          ;
  FIRST            : 'first'           ;
  FIRST_ROWS       : 'first_rows'      ;
  FLOAT            : 'float'           ;
  FLUSH            : 'flush'           ;
  FOR              : 'for'             ;
  FORM             : 'form'            ;
  FORMAT           : 'format'          ;
  FORMONLY         : 'formonly'        ;
  FOREACH          : 'foreach'         ; 
  FOUND            : 'found'           ;
  FRACTION         : 'fraction'        ;                 
  FREE             : 'free'            ;                 
  FROM             : 'from'            ; //SQL & PREPARE
  FUNCTION         : 'function'        ;
  GET_FLDBUF       : 'getfldbuf'       ;      
  GLOBALS          : 'globals'         ;       
  GO               : 'go'              ;       
  GOTO             : 'goto'            ;       
  GREEN 	   : 'green'           ;
  GROUP            : 'group'           ; //SQL
  HAVING           : 'having'          ; //SQL
  HEADER           : 'header'          ;         
  HELP             : 'help'            ;         
  HIDE             : 'hide'            ;
  HOLD             : 'hold'            ; //CURSOR
  HOUR             : 'hour'            ; //CURSOR
  IF               : 'if'              ;
  IN               : 'in'              ; //SQL
  INNER            : 'inner'           ; //SQL
  INDEX            : 'index'           ; //SQL
  INDICATOR        : 'indicator'       ; //SQL
  INFIELD          : 'infield'         ;      
  INITIALIZE       : 'initialize'      ; 
  INPUT            : 'input'           ; 
  INSERT           : 'insert'          ; //SQL
  INSTRUCTIONS     : 'instructions'    ; 
  INTO             : 'into'            ; //SQL & CURSOR 
  INT              : 'int'             ;
  INT_FLAG         : 'int_flag'        ;
  INTEGER          : 'integer'         ;
  INTERRUPT        : 'interrupt'       ;
  INTERVAL         : 'interval'        ;
  INVISIBLE        : 'invisible'       ; 
  IS               : 'is'              ;      
  ISNULL           : 'isnull'          ;      
  ISOLATION        : 'isolation'       ;
  JOIN             : 'join'            ;      
  KEY              : 'key'             ;
  LABEL            : 'label'           ;
  LAST             : 'last'            ;
  LEFT             : 'left'            ;
  LENGTH           : 'length'          ;      
  LET              : 'let'             ;
  LIKE             : 'like'            ;      
  LINE             : 'line'            ;      
  LINENO           : 'lineno'          ;      
  LINES            : 'lines'           ;      
  LOAD             : 'load'            ;      
  LOCATE           : 'locate'          ;
  LOCK             : 'lock'            ;
  LOG              : 'log'             ;
  LONG             : 'long'            ;
  MAGENTA          : 'magenta'         ;
  MATCHES          : 'matches'         ;      
  MENU             : 'menu'            ;
  MESSAGE          : 'message'         ;
  MAIN             : 'main'            ;
  MARGIN           : 'margin'          ;
  MAX              : 'max'             ;
  MDY              : 'mdy'             ;
  MIN              : 'min'             ;
  MINUTE           : 'minute'          ;
  MOD              : 'mod'             ;
  MODE             : 'mode'            ;
  MODULE           : 'module'          ;
  MONTH            : 'month'           ;
  MONEY            : 'money'           ;
  NCHAR            : 'nchar'           ;
  NAME             : 'name'            ;
  NEED             : 'need'            ;
  NEXT             : 'next'            ;
  NEW              : 'new'             ;
  NORMAL           : 'normal'          ;
  NO               : 'no'              ;
  NOT              : 'not'             ;
  NOTFOUND         : 'notfound'        ;
  NOW              : 'now'             ;
  NUMERIC          : 'numeric'         ;
  NULL             : 'null'            ;
  NVARCHAR         : 'nvarchar'        ; //SQL
  NVL              : 'nvl'             ;
  OF               : 'of'              ;
  OFF              : 'off'             ;
  ON               : 'on'              ;
  OPEN             : 'open'            ;
  OPTION           : 'option'          ;
  OPTIONS          : 'options'         ;
  OR               : 'or'              ;
  ORD              : 'ord'             ;
  ORDER            : 'order'           ; //SQL
  OUTPUT           : 'output'          ;
  OUTER            : 'outer'           ; //SQL
  OTHERWISE        : 'otherwise'       ;
  PAGE             : 'page'            ;      
  PAGENO           : 'pageno'          ;      
  PAUSE            : 'pause'           ;
  PERCENT          : 'percent'         ;
  PIPE             : 'pipe'            ;
  PRECISION        : 'precision'       ;
  PREPARE          : 'prepare'         ;
  PREVIOUS         : 'previous'        ;
  PRINT            : 'print'           ;
  PRINTER          : 'printer'         ;
  PROGRAM          : 'program'         ;
  PROMPT           : 'prompt'          ;
  PUT              : 'put'             ;
  QUIT             : 'quit'            ;
  QUIT_FLAG        : 'quit_flag'       ;
  RECORD           : 'record'          ;
  REAL             : 'real'            ;
  READ             : 'read'            ;
  RED              : 'red'             ;
  RELATIVE         : 'relative'        ;
  REMOVE           : 'remove'          ;
  REOPTIMIZATION   : 'reoptimization'  ;
  REPEATABLE       : 'repeatable'      ;
  REPEAT           : 'repeat'          ;
  REPORT           : 'report'          ;
  RESIZE           : 'resize'          ;
  RETURN           : 'return'          ;
  RETURNING        : 'returning'       ;
  REVERSE          : 'reverse'         ;
  RIGHT            : 'right'           ;
  ROLLBACK         : 'rollback'        ;
  ROW              : 'row'             ;
  ROWS             : 'rows'            ;
  RUN              : 'run'             ;
  SCREEN           : 'screen'          ;
  SCROLL           : 'scroll'          ;
  SECOND           : 'second'          ;
  SKIP             : 'skip'            ;
  SELECT           : 'select'          ;
  SET              : 'set'             ;
  SHARE            : 'share'           ;
  SHOW             : 'show'            ;
  SIZE             : 'size'            ;
  SLEEP            : 'sleep'           ;
  SMALLFLOAT       : 'smalfloat'       ;
  SMALLINT         : 'smallint'        ;
  SPACE            : 'space'           ;      
  SPACES           : 'spaces'          ;      
  SQL              : 'sql'             ;
  SQLERROR         : 'sqlerror'        ;
  SQLWARNING       : 'sqlwarning'      ;
  START            : 'start'           ;
  STABILITY        : 'stability'       ;
  STATISTICS       : 'statitics'       ;
  STATUS           : 'status'          ;
  STOP             : 'stop'            ;
  SUM              : 'sum'             ;
  TABLE            : 'table'           ;
  TABLES           : 'tables'          ;
  TERMINATE        : 'terminate'       ;
  TEMP             : 'temp'            ;
  TEXT             : 'text'            ;
  THEN             : 'then'            ;
  THROUGH          : 'through'         ;
  THRU             : 'thru'            ;
  TIME             : 'time'            ;      
  TO               : 'to'              ;
  TODAY            : 'today'           ;      
  TOP              : 'top'             ;
  TRAILER          : 'trailer'         ;
  TRUE             : 'true'            ;
  TYPE             : 'type'            ;
  UNCONSTRAINED    : 'unconstrained'   ;  
  UNDERLINE        : 'underline'       ;      
  UNION            : 'union'           ;      
  UNIQUE           : 'unique'          ;      
  UNITS            : 'units'           ;      
  UNLOAD           : 'unload'          ;      
  UP               : 'up'              ;
  UPDATE           : 'update'          ; //SQL
  USER             : 'user'            ;      
  USING            : 'using'           ;      
  VALIDATE         : 'validate'        ; 
  VALUES           : 'values'          ; //SQL
  VARCHAR          : 'varchar'         ; //SQL
  WEEKDAY          : 'weekday'         ;      
  VIEW             : 'view'            ; //SQL
  WAIT             : 'wait'            ;
  WAITING          : 'waiting'         ;
  WARNING          : 'warning'         ;
  WHEN             : 'when'            ;
  WHENEVER         : 'whenever'        ;
  WHERE            : 'where'           ; //SQL
  WHILE            : 'while'           ;
  WHITE            : 'white'           ;
  WITH             : 'with'            ;
  WITHOUT          : 'without'         ;
  WINDOW           : 'window'          ;
  WORDWRAP         : 'wordwrap'        ;      
  WORK             : 'work'            ;          
  YEAR             : 'year'            ;          
  YELLOW           : 'yellow'          ;          


//----------------------------------------------------------------------------
// OPERATORS
//----------------------------------------------------------------------------
PLUS            : '+'   ;
MINUS           : '-'   ;
STAR            : '*'   ;
SLASH           : '/'   ;
COMMA           : ','   ;
SEMI            : ';'   ;
COLON           : ':'   ;
EQUAL           : '='   ;
//NOT_EQUAL       : "<>" | "!="  ;

// Why did I do this?  Isn't this handle by just increasing the look ahead?
NOT_EQUAL       :
            	  '<' 
                 	(       (  '>'  )
                    	 |      (  '='  ) 
                    	)?
        	        | '!=' 
        	        | '^='
    		;
    		/*
LT              : '<'   ;
LE              : "<="  ;
GE              : ">="  ;
GT              : '>'   ;
*/
GT              : '>' 
                  ( '='  )? 
                ;

LPAREN          : '('   ;
RPAREN          : ')'   ;
LBRACK          : '['   ;
RBRACK          : ']'   ;
//DOT             : '.'   ;
ATSYMBOL        : '@'   ;
DOUBLEVERTBAR   : '||'   ;

// Whitespace -- ignored
WS	:	(	' '
		|	'\t'
		|	'\f'
		)+
		
	;

EOL
	:
		(	'\r\n'  // Evil DOS
		|	'\r'    // Macintosh
		|	'\n'    // Unix (the right way)
		)
		
	;

// Single-line comments
SL_COMMENT
	:	'#'
		(~('\n'|'\r'))* ('\n'|'\r'('\n')?)
		
	;

SL_COMMENT_2
	:	'--'
		(~('\n'|'\r'))* ('\n'|'\r'('\n')?)
		
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
	
	:	('a'..'z'|'_') ('a'..'z'|'_'|'0'..'9')*
	;
	
	// character literals
	/*
CHAR_LITERAL
	:	'\'' ( ESC | ~'\'' ) '\''
	;
	*/

// string literals
STRING_LITERAL
	:	('"') (/*ESC*|*/~('"'|'\\'))* ('"')
	|	('\'') (/*ESC|~*/('\\'|'\''))* ('\'')
	;


// a numeric literal
NUM_INT
	
	: '.' 
		(('0'..'9')+ )?
	|	(
		 ('0'..'9') ('0'..'9')* 		// non-zero decimal
		)
		// only check to see if it's a float if looks like decimal so far
		(	
			(	'.' ('0'..'9')* 
			)
			
		)?
	;


// a couple protected methods to assist in matching floating point numbers

EXPONENT
	:	('e') ('+'|'-')? ('0'..'9')+
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
	:	('0'..'9'|'a'..'f')
	;


// a dummy rule to force vocabulary to be all characters (except special
//   ones that ANTLR uses internally (0 to 2)

VOCAB
	:	'\1'..'\377'
	;

