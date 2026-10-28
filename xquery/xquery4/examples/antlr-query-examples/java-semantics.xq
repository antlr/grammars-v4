(:
literal
    : IntegerLiteral
    | FloatingPointLiteral
    | BooleanLiteral
    | CharacterLiteral
    | StringLiteral
    | TextBlock
    | NullLiteral
    ;
:)


declare record boolean-literal("type" as enum('true', 'false'));

declare function java:true($node as node()) {

};

declare function java:false($node as node()) {

};

declare function java:boolean-literal($node as node()) {
    if ($node=>string() = 'true')
        then java:true($node)
        else java:false($node)
};

declare function java:true() {
    {
        type: "true",
        nullable: false
    }
};

declare function java:false() {
    {
        type: "false",
        nullable: false
    }
};

declare function java:int-literal() {

};

declare function java:null() {
    {
        type: "null",
        nullable: true
    }
};

(: declare function accept($node) {
    typeswitch ($node){
        case element(IntegerLiteral)
            return java:int-literal($node)
        case element(FloatingPointLiteral)
            return java:float-literal($node)
        case element(BooleanLiteral)
            return java:boolean-literal($node)
        case element(CharacterLiteral)
            return java:character-literal($node)
        case element(StringLiteral)
            return java:string-literal($node)
        case element(TextBlock)
            return java:string-literal-from-text-block($node)
        case element(NullLiteral)
            return java:null()
        case element()
            return java:null()

        default return
    }

}; :)

(:
expression
    : lambdaExpression
    | assignmentExpression
    ;
:)

(:
primary
    : primaryNoNewArray
    | arrayCreationExpression
    ;
:)

(:
primaryNoNewArray
    : literal pNNA?
    | classLiteral pNNA?
    | 'this' pNNA?
    | typeName '.' 'this' pNNA?
    | '(' expression ')' pNNA?
    | unqualifiedClassInstanceCreationExpression pNNA?
    | expressionName '.' unqualifiedClassInstanceCreationExpression pNNA?
    | arrayCreationExpression '.' unqualifiedClassInstanceCreationExpression pNNA?
    | arrayCreationExpression '.' identifier pNNA?
    | 'super' '.' identifier pNNA?
    | typeName '.' 'super' '.' identifier pNNA?
    | expressionName '[' expression ']' pNNA?
    | arrayCreationExpressionWithInitializer '[' expression ']' pNNA?
    | methodName '(' argumentList? ')' pNNA?
    | typeName '.' typeArguments? identifier '(' argumentList? ')' pNNA?
    | expressionName '.' typeArguments? identifier '(' argumentList? ')' pNNA?
    | arrayCreationExpression '.' typeArguments? identifier '(' argumentList? ')' pNNA?
    | 'super' '.' typeArguments? identifier '(' argumentList? ')' pNNA?
    | typeName '.' 'super' '.' typeArguments? identifier '(' argumentList? ')' pNNA?
    | expressionName '::' typeArguments? identifier pNNA?
    | arrayCreationExpression '::' typeArguments? identifier pNNA?
    | referenceType '::' typeArguments? identifier pNNA?
    | 'super' '::' typeArguments? identifier pNNA?
    | typeName '.' 'super' '::' typeArguments? identifier pNNA?
    | classType '::' typeArguments? 'new' pNNA?
    | arrayType '::' 'new' pNNA?
    ;
:)

(:
pNNA
    : '.' unqualifiedClassInstanceCreationExpression pNNA?
    | '.' identifier pNNA?
    | '[' expression ']' pNNA?
    | '.' typeArguments? identifier '(' argumentList? ')' pNNA?
    | '::' typeArguments? identifier pNNA?
    ;
:)
(:
classLiteral
    : typeName ('[' ']')* '.' 'class'
    | numericType ( '[' ']')* '.' 'class'
    | 'boolean' ( '[' ']')* '.' 'class'
    | 'void' '.' 'class'
    ;
:)
()
