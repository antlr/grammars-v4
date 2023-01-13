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
    | emptyStatement_
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
    | propertyDeclaration
    ;

block
    : '{' statement* '}'
    ;

variableStatement
    : varModifier variables eos
    ;

variables
    : variable (',' variable)*
    ;

variable
    : Identifier ('=' expression)?
    ;

emptyStatement_
    : SemiColon
    ;

expressionStatement
    : expressions eos
    ;

ifStatement
    : If '(' expressions ')' statement (Else statement)?
    ;

iterationStatement
    : Do statement While '(' expressions ')' eos                                    # DoStatement
    | While '(' expressions ')' statement                                           # WhileStatement
    | For '(' expressions? ';' expressions? ';' expressions? ')' statement          # ForStatement
    | For '(' varModifier variables ';' expressions? ';' expressions? ')' statement # ForVarStatement
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
    : Return expressions? eos
    ;

withStatement
    : With '(' expressions ')' statement
    ;

switchStatement
    : Switch '(' expressions ')' '{' ( caseClause | defaultClause )* '}'
    ;

caseClause
    : Case expressions ':' statement*
    ;

defaultClause
    : Default ':' statement*
    ;

throwStatement
    : Throw expressions eos
    ;

tryStatement
    : Try block Catch ('(' Identifier? ')')? block
    ;

debuggerStatement
    : Debugger eos
    ;

functionDeclaration
    : Function_ Identifier ('(' functionParameters? ')')? block
    ;

anoymousFunctionDeclaration
    : Function_ ('(' functionParameters? ')')? block
    ;

classDeclaration
    : Class Identifier (Extends expression (',' expression)* )? block
    ;

propertyDeclaration
    : Property Identifier '{'
        ( Getter ('(' ')')? block
        | Setter '(' functionParameter ')' block
    )* '}'
    ;

functionParameters
    : functionParameter (',' functionParameter)*
    ;

functionParameter
    : Identifier ('=' expression)?
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

expressions
    : expression (',' expression)*
    ;

// TODO: can we optimize it?
expression
    : anoymousFunctionDeclaration                           # FunctionExpression
    | expression '[' expressions ']'                        # MemberIndexExpression
    | expression '.' identifierName                         # MemberDotExpression
    | '.' identifierName                                    # WithDotExpression
    | expression arguments                                  # CallExpression
    | New expression arguments?                             # NewExpression
    | '&' expression                                        # ReferenceExpression
    | '*' expression                                        # PointerExpression
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
    | expression In expression                              # InExpression          // TODO: any standard for it?
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
    | '(' expressions ')'                                   # ParenthesizedExpression
    | expression If expressions                             # IfExpression          // b = c = 1 if a ==> (b=c=1)if a
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
    | OctetLiteral
    | RegularExpressionLiteral
    | numericLiteral
    | Void
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
    | Function_
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
