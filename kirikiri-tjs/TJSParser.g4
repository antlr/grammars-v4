parser grammar TJSParser;

options {
    tokenVocab=TJSLexer;
    superClass=TJSBaseParser; 
}

program
    : statement* EOF
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
    | function
    | property
    ;

block
    : '{' statement* '}'
    ;

variableStatement
    : varModifier variableDeclarations eos
    ;

variableDeclarations
    : variableDeclaration (',' variableDeclaration)*
    ;

variableDeclaration
    : Identifier ('=' expression)?
    ;

emptyStatement
    : SemiColon
    ;

expressionStatement
    : expressionSequence eos
    ;

ifStatement
    : If '(' expressionSequence ')' statement (Else statement)?
    ;

iterationStatement
    : Do statement While '(' expressionSequence ')' eos                                                         # DoStatement
    | While '(' expressionSequence ')' statement                                                                # WhileStatement
    | For '(' expressionSequence? ';' expressionSequence? ';' expressionSequence? ')' statement                 # ForStatement
    | For '(' varModifier variableDeclarations ';' expressionSequence? ';' expressionSequence? ')' statement # ForVarStatement
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
    : Switch '(' expressionSequence ')' '{' ( caseClause | defaultClause )* '}'
    ;

caseClause
    : Case expressionSequence ':' statement*
    ;

defaultClause
    : Default ':' statement*
    ;

throwStatement
    : Throw expressionSequence eos
    ;

tryStatement
    : Try block catchProduction
    ;

catchProduction
    : Catch ('(' Identifier? ')')? block
    ;

debuggerStatement
    : Debugger eos
    ;

function
    : Function Identifier? ('(' functionParameters? ')')? block
    ;

classDeclaration
    : Class Identifier (Extends expression (',' expression)* )? block
    ;

property // TJS Property define
    : Property Identifier '{'
        ( Getter ('(' ')')? block
        | Setter '(' functionParameter ')' block
    )* '}'
    ;

functionParameters
    : functionParameter (',' functionParameter)*
    ;

functionParameter
    : Identifier ('=' expression)?      // TJS2, ECMAScript 6: Initialization
    | Identifier? '*'
    ;

arrayLiteral
    : varModifier? '[' arrayElementSeprator* elementList? arrayElementSeprator* ']'
    ;

elementList
    : expression (arrayElementSeprator+ expression)*
    ;

arrayElementSeprator
    : ','
    | '=>'
    ;

objectLiteral
    : varModifier? '%[' objectProperty? (',' objectProperty)* ','?']'
    ;

objectProperty
    : expression (':' |'=' | '=>' | ',') expression // TJS resolve object key in runtime
    ;

arguments
    : '(' argument? (',' argument?)* ')'
    | '(' Ellipsis ')' // ... means use argument (JS function argument)
    ;

argument
    : expression '*'?
    | '*'
    ;

expressionSequence
    : expression (',' expression)*
    ;

expression
    : function                                   # FunctionExpression
    | expression '[' expressionSequence ']'                 # MemberIndexExpression
    | expression '.' identifierName                         # MemberDotExpression
    | '.' identifierName                                    # WithDotExpression
    | expression arguments                                  # ArgumentsExpression
    | New expression arguments?                             # NewExpression
    | '&' expression                                        # TJSRefExpression
    | '*' expression                                        # TJSPtrExpression
    | expression '<->' expression                           # SwapExpression

    | expression '!'                                        # EvalExpression        // t.f incontextof 's'! => 0721 (s=%[v=>0721])
    | expression Incontextof expression                     # InContextOfExpression // a incontextof d++ => Error: object++
    | expression '++'                                       # PostIncrementExpression
    | expression '--'                                       # PostDecreaseExpression
    | expression Isvalid                                    # IsValidExpression     // t incontextof ctx isvalid => 1
    | Delete expression                                     # DeleteExpression      // delete a.c isvalid => error
    | Isvalid expression                                    # IsValidExpression     // isvalid delete a.c => 1
    | Typeof expression                                     # TypeofExpression      // typeof 1 instanceof "String" => 'Integer'
    | expression Instanceof expression                      # InstanceofExpression  // 1 instanceof "Number" isvalid => 0,isvalid 1 instanceof "String"=>1
    | Invalidate expression                                 # InvalidateExpression  // invalidate a instanceof "Number" => 0
    | '++' expression                                       # PreIncrementExpression
    | '--' expression                                       # PreDecreaseExpression // typeof 1 + 1 = 'Integer1'
    | '+' expression                                        # UnaryPlusExpression
    | '-' expression                                        # UnaryMinusExpression
    | '~' expression                                        # BitNotExpression
    | '!' expression                                        # NotExpression
    | expression ('*' | '/' | '%' | '\\') expression        # MultiplicativeExpression
    | expression ('+' | '-') expression                     # AdditiveExpression
    | expression ('<<' | '>>' | '>>>') expression           # BitShiftExpression
    | expression ('<' | '>' | '<=' | '>=') expression       # RelationalExpression
    | expression In expression                              # InExpression          // TODO: any standard for it?
    | expression ('==' | '!=' | '===' | '!==') expression   # EqualityExpression
    | expression '&' expression                             # BitAndExpression
    | expression '^' expression                             # BitXOrExpression
    | expression '|' expression                             # BitOrExpression
    | expression '&&' expression                            # LogicalAndExpression
    | expression '||' expression                            # LogicalOrExpression
    | expression '?' expression ':' expression              # TernaryExpression
    | expression '=' expression                             # AssignmentExpression
    | expression assignmentOperator expression              # AssignmentOperatorExpression
    | This                                                  # ThisExpression
    | Identifier                                            # IdentifierExpression
    | Super                                                 # SuperExpression
    | typeConverter expression                              # TypeConvertExpression
    | literal                                               # LiteralExpression
    | arrayLiteral                                          # ArrayLiteralExpression
    | objectLiteral                                         # ObjectLiteralExpression
    | '(' expressionSequence ')'                            # ParenthesizedExpression
    | expression If expressionSequence                      # TJSIfExpression       // b = c = 1 if a ==> (b=c=1)if a
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
    | (StringLiteral | TemplateStringLiteral)+
    | octetLiteral
    | RegularExpressionLiteral
    | numericLiteral
    | Void
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
    ;
