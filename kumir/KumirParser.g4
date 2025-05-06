// KumirParser.g4
// ANTLR v4 Parser Grammar for the Kumir language.
// This version correctly handles complex algorithm names (including keywords/numbers)
// by using a specific rule `algorithmNameTokens` in the header.
// Source: Refined based on official documentation and extensive testing
//         against K.Y. Polyakov's examples. Developed collaboratively.
// Author: [Your Name/GitHub Handle]
// License: MIT License

parser grammar KumirParser;

options { tokenVocab=KumirLexer; } // Use tokens from KumirLexer.g4

/*
 * =============================================================================
 * Helper Rules: Literals, Identifiers, Expressions
 * (Defined first as they are used by higher-level rules)
 * =============================================================================
 */
qualifiedIdentifier
    : ID // Currently simple ID, can be extended for module.member later
    ;

literal
    : INTEGER | REAL | STRING | CHAR_LITERAL | TRUE | FALSE | colorLiteral | NEWLINE_CONST
    ;

colorLiteral
    : PROZRACHNIY | BELIY | CHERNIY | SERIY | FIOLETOVIY | SINIY | GOLUBOY | ZELENIY | ZHELTIY | ORANZHEVIY | KRASNIY
    ;

expressionList // Used inside array literals {}
    : expression (COMMA expression)*
    ;

arrayLiteral // e.g., {1, 2, 3}
    : LBRACE expressionList? RBRACE
    ;

// Expressions are defined from lowest to highest precedence
primaryExpression // The most basic elements
    : literal
    | qualifiedIdentifier // Variable or function call without arguments
    | RETURN_VALUE        // 'знач' keyword
    | LPAREN expression RPAREN // Parenthesized expression
    | arrayLiteral
    ;

argumentList // e.g., (a, b+c, 5)
    : expression (COMMA expression)*
    ;

indexList // e.g., [i], [i, j], [k:m]
    : expression (COLON expression)? // Single index or slice
    | expression COMMA expression    // 2D index
    ;

postfixExpression // Handles array/string access and function calls after a primary expression
    : primaryExpression ( LBRACK indexList RBRACK | LPAREN argumentList? RPAREN )*
    ;

unaryExpression // Handles unary plus, minus, not
    : (PLUS | MINUS | NOT) unaryExpression | postfixExpression
    ;

powerExpression // Handles exponentiation (**)
    : unaryExpression (POWER powerExpression)?
    ;

multiplicativeExpression // Handles *, /
    : powerExpression ((MUL | DIV) powerExpression)*
    ;

additiveExpression // Handles +, -
    : multiplicativeExpression ((PLUS | MINUS) multiplicativeExpression)*
    ;

relationalExpression // Handles <, >, <=, >=
    : additiveExpression ((LT | GT | LE | GE) additiveExpression)*
    ;

equalityExpression // Handles =, <>
    : relationalExpression ((EQ | NE) relationalExpression)*
    ;

logicalAndExpression // Handles 'и' (AND)
    : equalityExpression (AND equalityExpression)*
    ;

logicalOrExpression // Handles 'или' (OR)
    : logicalAndExpression (OR logicalAndExpression)*
    ;

expression // Top-level expression rule
    : logicalOrExpression
    ;

/*
 * =============================================================================
 * Data Types and Variable Declarations
 * =============================================================================
 */
typeSpecifier // Defines the type of a variable or parameter
    : arrayType              // e.g., целтаб
    | basicType TABLE_SUFFIX?// e.g., цел, лит таб (if not matched as arrayType by lexer)
    | actorType              // e.g., цвет, файл
    ;

basicType // Basic built-in types
    : INTEGER_TYPE | REAL_TYPE | BOOLEAN_TYPE | CHAR_TYPE | STRING_TYPE
    ;

actorType // Actor-specific types
    : KOMPL_TYPE | COLOR_TYPE | SCANCODE_TYPE | FILE_TYPE
    ;

arrayType // Explicit array type tokens from lexer
    : INTEGER_ARRAY_TYPE | REAL_ARRAY_TYPE | BOOLEAN_ARRAY_TYPE | CHAR_ARRAY_TYPE | STRING_ARRAY_TYPE
    ;

arrayBounds // e.g., [1:N]
    : expression COLON expression
    ;

// Describes a single item in a variable declaration list, allowing optional initialization with '='
variableDeclarationItem
    : ID (LBRACK arrayBounds (COMMA arrayBounds)* RBRACK)? ( EQ expression )?
    ;

// List of variables in a declaration, e.g., i, j, k
variableList
    : variableDeclarationItem (COMMA variableDeclarationItem)*
    ;

// Local variable declaration (inside нач...кон)
variableDeclaration
    : typeSpecifier variableList
    ;

// Global variable declaration (outside algorithms)
globalDeclaration
    : typeSpecifier variableList SEMICOLON?
    ;

// Global assignment (using :=)
globalAssignment
    : qualifiedIdentifier ASSIGN (literal | unaryExpression | arrayLiteral) SEMICOLON?
    ;

/*
 * =============================================================================
 * Algorithm Parameters
 * =============================================================================
 */
// Single parameter declaration (e.g., арг цел n, рез лит s)
parameterDeclaration
    : (IN_PARAM | OUT_PARAM | INOUT_PARAM)? typeSpecifier variableList
    ;

// List of parameters in parentheses
parameterList
    : parameterDeclaration (COMMA parameterDeclaration)*
    ;

/*
 * =============================================================================
 * Algorithm Names
 * =============================================================================
 */
/**
 * algorithmNameTokens: Captures all tokens that constitute the algorithm name.
 * It consumes any token (.) as long as the lookahead (LA(1)) is not one of the
 * stop tokens (LPAREN, ALG_BEGIN, PRE_CONDITION, POST_CONDITION, SEMICOLON, EOF).
 * This allows names like "Сумма И счетчик" or "Факториал 100".
 * The `+` ensures at least one token is consumed for the name.
 */
algorithmNameTokens
    : ~(LPAREN | ALG_BEGIN | PRE_CONDITION | POST_CONDITION | SEMICOLON | EOF)+
    ;

/**
 * algorithmName: Used only for checking the name after the 'кон' keyword.
 * Kept stricter (ID+) for this purpose.
 */
algorithmName: ID+ ;


/*
 * =============================================================================
 * Algorithm Structure
 * =============================================================================
 */
/**
 * algorithmHeader: Defines the algorithm header.
 * Structure: алг [type]? NAME [(parameters)]? [;]?
 * Uses algorithmNameTokens to capture the potentially complex name.
 */
algorithmHeader
    : ALG_HEADER (typeSpecifier)? algorithmNameTokens (LPAREN parameterList? RPAREN)? SEMICOLON?
    ;

// Pre-condition block (дано)
preCondition
    : PRE_CONDITION expression SEMICOLON?
    ;

// Post-condition block (надо)
postCondition
    : POST_CONDITION expression SEMICOLON?
    ;

// Algorithm body (sequence of statements)
algorithmBody
    : statementSequence
    ;

// Sequence of statements
statementSequence
    : statement*
    ;

/**
 * lvalue: Left-hand side of an assignment (what is being assigned to).
 * Can be a variable, array element, or the special 'знач'.
 */
lvalue
    : qualifiedIdentifier (LBRACK indexList RBRACK)? // Variable or array element
    | RETURN_VALUE                                   // 'знач' for function return
    ;

// Assignment statement (using :=) or an expression statement (like a procedure call)
assignmentStatement
    : lvalue ASSIGN expression
    | expression
    ;

// Argument for I/O statements (вывод/ввод)
ioArgument
    : expression (COLON expression (COLON expression)?)? // Expression with optional formatting
    | NEWLINE_CONST // 'нс'
    ;

ioArgumentList
    : ioArgument (COMMA ioArgument)*
    ;

ioStatement
    : INPUT ioArgumentList
    | OUTPUT ioArgumentList
    ;

// If statement (если ... то ... иначе ... все)
ifStatement
    : IF expression THEN statementSequence (ELSE statementSequence)? FI
    ;

// Case block inside a switch (при условие : ...)
caseBlock
    : CASE expression COLON statementSequence
    ;

// Switch statement (выбор ... при ... иначе ... все)
switchStatement
    : SWITCH caseBlock+ (ELSE statementSequence)? FI
    ;

// Loop ending condition (кц при ...)
endLoopCondition
    : ENDLOOP_COND expression
    ;

// Loop specifiers (для, пока, N раз)
loopSpecifier
    : FOR ID FROM expression TO expression (STEP expression)?
    | WHILE expression
    | expression TIMES
    ;

// Loop statement (нц ... кц)
loopStatement
    : LOOP loopSpecifier? statementSequence (ENDLOOP | endLoopCondition)
    ;

// Exit statement (выход)
exitStatement
    : EXIT
    ;

// Pause statement (пауза)
pauseStatement
    : PAUSE
    ;

// Stop statement (стоп)
stopStatement
    : STOP
    ;

// Assertion statement (утв)
assertionStatement
    : ASSERTION expression
    ;

// Procedure call statement (can also be handled via expression)
procedureCallStatement
    : qualifiedIdentifier (LPAREN argumentList? RPAREN)?
    ;

// General statement rule
statement
    : variableDeclaration SEMICOLON?
    | assignmentStatement SEMICOLON?
    | ioStatement SEMICOLON?
    | ifStatement SEMICOLON?
    | switchStatement SEMICOLON?
    | loopStatement SEMICOLON?
    | exitStatement SEMICOLON?
    | pauseStatement SEMICOLON?
    | stopStatement SEMICOLON?
    | assertionStatement SEMICOLON?
    | procedureCallStatement SEMICOLON?
    | SEMICOLON // Allow empty statements
    ;

/**
 * algorithmDefinition: Defines a complete algorithm structure.
 */
algorithmDefinition
    : algorithmHeader (preCondition | postCondition | variableDeclaration)*
      ALG_BEGIN
      algorithmBody
      ALG_END (algorithmName)? SEMICOLON? // Use stricter 'algorithmName' for check after 'кон'
    ;

/*
 * =============================================================================
 * Module Structure
 * =============================================================================
 */
moduleName
    : qualifiedIdentifier
    | STRING
    ;

importStatement
    : IMPORT moduleName SEMICOLON?
    ;

// Item that can appear at the program/module top level or inside module body
programItem
    : importStatement
    | globalDeclaration
    | globalAssignment
    ;

moduleHeader
    : MODULE qualifiedIdentifier SEMICOLON?
    ;

moduleBody // Body of an explicit module
    : (programItem | algorithmDefinition)*
    ;

implicitModuleBody // Content of a file without explicit 'модуль' header/footer
    : (programItem | algorithmDefinition)+
    ;

moduleDefinition // Either an explicit or implicit module structure
    : moduleHeader moduleBody ENDMODULE (qualifiedIdentifier)? SEMICOLON?
    | implicitModuleBody
    ;

/*
 * =============================================================================
 * Program Structure (Start Rule)
 * =============================================================================
 */
/**
 * program: The main entry point for the grammar.
 * A program consists of optional top-level items (imports, globals)
 * followed by one or more module or algorithm definitions.
 */
program
    : programItem* (moduleDefinition | algorithmDefinition)+ EOF
    ;

// End of KumirParser.g4