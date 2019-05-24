parser grammar TJSParser;

options {
    tokenVocab=TJSLexer;
    superClass=TJSBaseParser; 
}

program
    : statements? EOF
    ;

statement
    : block
    | variableStatement
    | emptyStatement
    | classDeclaration
    | expressionStatement
    | ifStatement
    | iterationStatement
    | continueStatement
    | breakStatement
    | returnStatement
    | withStatement
    | switchStatement
    | throwStatement
    | tryStatement
    | debuggerStatement
    | functionDeclaration
    | propertyDefinition
    ;

block
    : '{' statementList? '}'
    ;

statementList
    : statement+
    ;

variableStatement
    : varModifier variableDeclarationList eos
    ;

variableDeclarationList
    : variableDeclaration (',' variableDeclaration)*
    ;

variableDeclaration
    : Identifier ('=' varModifier? singleExpression)? // ECMAScript 6: Array & Object Matching
    ;

emptyStatement
    : SemiColon
    ;

expressionStatement
    : {this.notOpenBraceAndNotFunction()}? expressionSequence eos
    ;

ifStatement
    : If '(' expressionSequence ')' statement (Else statement)?
    ;


iterationStatement
    : Do statement While '(' expressionSequence ')' eos                                                         # DoStatement
    | While '(' expressionSequence ')' statement                                                                # WhileStatement
    | For '(' expressionSequence? ';' expressionSequence? ';' expressionSequence? ')' statement                 # ForStatement
    | For '(' varModifier variableDeclarationList ';' expressionSequence? ';' expressionSequence? ')' statement # ForVarStatement
    ;

varModifier
    : Var | Const
    |'('(  Var | Const )')'
    ;

typeConverter
    : '('(Int | Real | String )')'
    | Int
    | Real
    | String
    ;

continueStatement
    : Continue eos
    ;

breakStatement
    : Break eos
    ;

returnStatement
    : Return expressionSequence? eos
    ;

withStatement
    : With '(' expressionSequence ')' statement
    ;

switchStatement
    : Switch '(' expressionSequence ')' caseBlock
    ;

caseBlock
    : '{' caseClauses? (defaultClause caseClauses?)? '}'
    ;

caseClauses
    : caseClause+
    ;

caseClause
    : Case expressionSequence ':' statementList?
    ;

defaultClause
    : Default ':' statementList?
    ;

throwStatement
    : Throw expressionSequence eos
    ;

tryStatement
    : Try block (catchProduction finallyProduction? | finallyProduction)
    ;

catchProduction
    : Catch ('(' Identifier? ')')? block
    ;

finallyProduction
    : Finally block
    ;

debuggerStatement
    : Debugger eos
    ;

functionDeclaration
    : Function Identifier? ('(' functionParameters? ')')? ('{' functionBody '}')?
    ;

classDeclaration
    : Class Identifier? classTail
    ;

classTail // Another strange behavior for TJS: we can write almost any statement in TJS class body
    : (Extends singleExpression (',' singleExpression)* )? '{' statement* '}'
    ;

propertyDefinition // TJS Property define
    : Property Identifier '{' (tjsGetter|tjsSetter)* '}'
    ;

tjsGetter
    : Getter ('(' ')')? '{' functionBody '}'
    ;

tjsSetter
    : Setter '(' functionParameters? ')' '{' functionBody '}'
    ;

methodDefinition
    : propertyName '(' functionParameters? ')' '{' functionBody '}'
    ;

functionParameters
    : functionParameter (',' functionParameter)*
    ;

functionParameter
    : Identifier ('=' singleExpression)?      // TJS2, ECMAScript 6: Initialization
    | Identifier? '*'
    ;

functionBody
    : statements?
    ;

statements
    : statement+
    ;

arrayLiteral
    : varModifier? '[' ','* elementList? ','* ']'
    ;

elementList
    : arrayElement (','+ arrayElement)*
    ;

arrayElement
    : singleExpression
    | singleExpression '=>' singleExpression
    ;

objectLiteral
    : varModifier? '%[' (propertyAssignment (',' propertyAssignment)*)? ','? ']'
    ;

propertyAssignment
    : propertyName (':' |'=' | '=>' | ',') singleExpression # PropertyExpressionAssignment  // {a:b,c:d} in TJS can be write like %[a,b,c,d], weird right?
    | singleExpression '=>' singleExpression                # InPropertyCall                // another weird behavior, need investigate
    ;

propertyName
    : identifierName
    | StringLiteral
    | numericLiteral
    ;

arguments
    : '(' tjsArgument? (',' tjsArgument?)* ')'
    | '(' ( Ellipsis | '*') ')' // ... means use argument (JS function argument)
    ;

tjsArgument // In TJS, argument can expanded by using *
    : singleExpression '*'?
    | '*'
    ;

expressionSequence
    : singleExpression (',' singleExpression)*
    ;

singleExpression
    : functionDeclaration                                                       # FunctionExpression
    | classDeclaration                                                          # ClassExpression
    | singleExpression '[' expressionSequence ']'                               # MemberIndexExpression
    | singleExpression '.' identifierName                                       # MemberDotExpression
    | singleExpression arguments                                                # ArgumentsExpression
    | New singleExpression arguments?                                           # NewExpression
    | singleExpression '++'                                                     # PostIncrementExpression
    | singleExpression '--'                                                     # PostDecreaseExpression
    | Delete singleExpression                                                   # DeleteExpression
    | Void singleExpression?                                                    # VoidExpression
    | Typeof singleExpression                                                   # TypeofExpression
    | '++' singleExpression                                                     # PreIncrementExpression
    | '--' singleExpression                                                     # PreDecreaseExpression
    | '+' singleExpression                                                      # UnaryPlusExpression
    | '-' singleExpression                                                      # UnaryMinusExpression
    | '~' singleExpression                                                      # BitNotExpression
    | '!' singleExpression                                                      # NotExpression
    | singleExpression '!'                                                      # EvalExpression
    | singleExpression ('*' | '/' | '%' | '\\') singleExpression                # MultiplicativeExpression
    | singleExpression ('+' | '-') singleExpression                             # AdditiveExpression
    | singleExpression ('<<' | '>>' | '>>>') singleExpression                   # BitShiftExpression
    | singleExpression ('<' | '>' | '<=' | '>=') singleExpression               # RelationalExpression
    | singleExpression Instanceof singleExpression                              # InstanceofExpression
    | singleExpression In singleExpression                                      # InExpression
    | singleExpression ('==' | '!=' | '===' | '!==') singleExpression           # EqualityExpression
    | singleExpression '&' singleExpression                                     # BitAndExpression
    | singleExpression '^' singleExpression                                     # BitXOrExpression
    | singleExpression '|' singleExpression                                     # BitOrExpression
    | singleExpression '&&' singleExpression                                    # LogicalAndExpression
    | singleExpression '||' singleExpression                                    # LogicalOrExpression
    | singleExpression '?' singleExpression ':' singleExpression                # TernaryExpression
    | singleExpression '=' singleExpression                                     # AssignmentExpression
    | singleExpression '<->' singleExpression                                   # SwapExpression
    | singleExpression assignmentOperator singleExpression                      # AssignmentOperatorExpression
    | singleExpression TemplateStringLiteral+                                   # TemplateStringExpression  // ECMAScript 6, TJS has simillar feature
    | This                                                                      # ThisExpression
    | Identifier                                                                # IdentifierExpression
    | Super                                                                     # SuperExpression
    | typeConverter singleExpression                                            # TypeConvertExpression
    | literal                                                                   # LiteralExpression
    | arrayLiteral                                                              # ArrayLiteralExpression
    | objectLiteral                                                             # ObjectLiteralExpression
    | '(' expressionSequence ')'                                                # ParenthesizedExpression
    | singleExpression Incontextof singleExpression                             # InContextOfExpression
    | singleExpression If singleExpression                                      # TJSIfExpression
    | Invalidate (('(' identifierName ')') |identifierName)                     # InvalidateExpression
    | '&' singleExpression                                                      # TJSRefExpression
    | '*' singleExpression                                                      # TJSPtrExpression
    | '.' identifierName                                                        # WithDotExpression
    | singleExpression Isvalid                                                  # IsValidExpression
    | Isvalid singleExpression                                                  # IsValidExpression
    ;

assignmentOperator
    : '*='
    | '/='
    | '%='
    | '+='
    | '-='
    | '<<='
    | '>>='
    | '>>>='
    | '&='
    | '^='
    | '|='
    | '\\='
    | '&&='
    | '||='
    ;

literal
    : NullLiteral
    | BooleanLiteral
    | tjsStringLiteral
    | octetLiteral
    | RegularExpressionLiteral
    | numericLiteral
    ;

tjsStringLiteral // tjs can use C like string literal: "part1""part2"
    : (StringLiteral
    | TemplateStringLiteral)+
    ;
octetLiteral
    : '<%'                  // Hack for octet literal
    ( NonIdentHexByte       // 0f 7a 08.....
    | OctalIntegerLiteral   // 00 07 .....
    | DecimalLiteral        // 13 75 .....
    | Identifier)*          // ef b9 .....
    '%>'                    // Remember handle it in type recognization
    ;

numericLiteral
    : DecimalLiteral
    | HexIntegerLiteral
    | OctalIntegerLiteral
    | OctalIntegerLiteral2
    | BinaryIntegerLiteral
    ;

identifierName
    : Identifier
    | reservedWord
    ;

reservedWord
    : keyword
    | NullLiteral
    | BooleanLiteral
    ;

keyword
    : Break
    | Do
    | Instanceof
    | Typeof
    | Case
    | Else
    | New
    | Var
    | Catch
    | Finally
    | Return
    | Void
    | Continue
    | For
    | Switch
    | While
    | Debugger
    | Function
    | This
    | With
    | Default
    | If
    | Throw
    | Delete
    | In
    | Try

    | Class
    | Enum
    | Extends
    | Super
    | Const
    | Export
    | Import
    | Static

    | Incontextof
    ;

eos
    : SemiColon
    | EOF
    | {this.lineTerminatorAhead()}?
    | {this.closeBrace()}?
    ;
