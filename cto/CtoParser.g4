parser grammar CtoParser;

options { tokenVocab=CtoLexer; }

modelUnit
    : namespaceDeclaration? importDeclaration* typeDeclaration* EOF
    ;

namespaceDeclaration
    : NAMESPACE qualifiedName
    ;

importDeclaration
    : IMPORT qualifiedName (DOT MUL)?
    ;

typeDeclaration
    : (assetDeclaration
    | conceptDeclaration  
    | enumDeclaration
    | participantDeclaration
    | transactionDeclaration
    | eventDeclaration)
    ;

classModifier
    : decorator
    | ABSTRACT
    ;

assetDeclaration
    : classModifier*
      ASSET IDENTIFIER
      (EXTENDS IDENTIFIER)?
      IDENTIFIED
      IDENTIFIER
      classBody
    ;

conceptDeclaration
    : classModifier*
      CONCEPT IDENTIFIER
      (EXTENDS IDENTIFIER)?
      classBody
    ;

enumDeclaration
    : ENUM IDENTIFIER LBRACE enumConstant* RBRACE;

enumConstant
    : VAR IDENTIFIER;

eventDeclaration
    : EVENT IDENTIFIER
      classBody
    ;

participantDeclaration
    : classModifier*
      PARTICIPANT IDENTIFIER
      (EXTENDS IDENTIFIER)?
      IDENTIFIED
      IDENTIFIER
      classBody
    ;

transactionDeclaration
    : classModifier*
      TRANSACTION IDENTIFIER
      classBody
    ;

classBody
    : LBRACE classBodyDeclaration* RBRACE;

classBodyDeclaration
    : ';'
    | fieldDeclaration
    ;

fieldDeclaration
    : fieldType IDENTIFIER 
    | refType identifier;

fieldType
    : VAR (primitiveType | IDENTIFIER) (LBRACK RBRACK)*;

refType
    : REF IDENTIFIER (LBRACK RBRACK)*;

identifier: IDENTIFIER | ASSET;

variableDeclaratorId
    : IDENTIFIER ('[' ']')*;

qualifiedNameList
    : qualifiedName (',' qualifiedName)*;

qualifiedName
    : IDENTIFIER ('.' IDENTIFIER)*;

literal
    : integerLiteral
    | floatLiteral
    | CHAR_LITERAL
    | STRING_LITERAL
    | BOOL_LITERAL
    | NULL_LITERAL
    ;

integerLiteral
    : DECIMAL_LITERAL
    | HEX_LITERAL
    | OCT_LITERAL
    | BINARY_LITERAL
    ;

floatLiteral
    : FLOAT_LITERAL
    | HEX_FLOAT_LITERAL
    ;

// ANNOTATIONS

decorator
    : AT qualifiedName ('(' elementValuePair ')')?;

elementValuePair
    : literal ',' (literal | IDENTIFIER);

primitiveType
    : BOOLEAN
    | DATE_TIME
    | DOUBLE
    | INTEGER
    | LONG
    | STRING
    ;

