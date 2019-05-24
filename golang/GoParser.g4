parser grammar GoParser;

options { tokenVocab=GoLexer; }

@parser::members
{/** * Returns {@code true} iff on the current index of the parser's
 * token stream a token exists on the {@code HIDDEN} channel which
 * either is a line terminator, or is a multi line comment that
 * contains a line terminator.
 *
 * @return {@code true} iff on the current index of the parser's
 * token stream a token exists on the {@code HIDDEN} channel which
 * either is a line terminator, or is a multi line comment that
 * contains a line terminator.
 */
private boolean lineTerminatorAhead() {
    // Get the token ahead of the current index.
    int possibleIndexEosToken = this.getCurrentToken().getTokenIndex() - 1;
    Token ahead = _input.get(possibleIndexEosToken);
    if (ahead.getChannel() != Lexer.HIDDEN) {
        // We're only interested in tokens on the HIDDEN channel.
        return false;
    }

    if (ahead.getType() == TERMINATOR) {
        // There is definitely a line terminator ahead.
        return true;
    }

    if (ahead.getType() == WS) {
        // Get the token ahead of the current whitespaces.
        possibleIndexEosToken = this.getCurrentToken().getTokenIndex() - 2;
        ahead = _input.get(possibleIndexEosToken);
    }

    // Get the token's text and type.
    String text = ahead.getText();
    int type = ahead.getType();

    // Check if the token is, or contains a line terminator.
    return (type == COMMENT && (text.contains("\r") || text.contains("\n"))) ||
            (type == TERMINATOR);
}

 /**
 * Returns {@code true} if no line terminator exists between the specified
 * token offset and the prior one on the {@code HIDDEN} channel.
 *
 * @return {@code true} if no line terminator exists between the specified
 * token offset and the prior one on the {@code HIDDEN} channel.
 */
private boolean noTerminatorBetween(int tokenOffset) {
    BufferedTokenStream stream = (BufferedTokenStream)_input;
    List<Token> tokens = stream.getHiddenTokensToLeft(stream.LT(tokenOffset).getTokenIndex());

    if (tokens == null) {
        return true;
    }

    for (Token token : tokens) {
        if (token.getText().contains("\n"))
            return false;
    }

    return true;
}

 /**
 * Returns {@code true} if no line terminator exists after any encounterd
 * parameters beyond the specified token offset and the next on the
 * {@code HIDDEN} channel.
 *
 * @return {@code true} if no line terminator exists after any encounterd
 * parameters beyond the specified token offset and the next on the
 * {@code HIDDEN} channel.
 */
private boolean noTerminatorAfterParams(int tokenOffset) {
    BufferedTokenStream stream = (BufferedTokenStream)_input;
    int leftParams = 1;
    int rightParams = 0;
    String value;

    if (stream.LT(tokenOffset).getText().equals("(")) {
        // Scan past parameters
        while (leftParams != rightParams) {
            tokenOffset++;
            value = stream.LT(tokenOffset).getText();

            if (value.equals("(")) {
                leftParams++;
            }
            else if (value.equals(")")) {
                rightParams++;
            }
        }

        tokenOffset++;
        return noTerminatorBetween(tokenOffset);
    }

    return true;
}}

//SourceFile       = PackageClause ";" { ImportDecl ";" } { TopLevelDecl ";" } .
sourceFile
    : packageClause eos ( importDecl eos )* ( topLevelDecl eos )*
    ;

//PackageClause  = "package" PackageName .
//PackageName    = identifier .
packageClause
    : 'package' IDENTIFIER
    ;

importDecl
    : 'import' ( importSpec | '(' ( importSpec eos )* ')' )
    ;

importSpec
    : ( '.' | IDENTIFIER )? importPath
    ;

importPath
    : STRING_LIT
    ;

//TopLevelDecl  = Declaration | FunctionDecl | MethodDecl .
topLevelDecl
    : declaration
    | functionDecl
    | methodDecl
    ;

//Declaration   = ConstDecl | TypeDecl | VarDecl .
declaration
    : constDecl
    | typeDecl
    | varDecl
    ;


//ConstDecl      = "const" ( ConstSpec | "(" { ConstSpec ";" } ")" ) .
constDecl
    : 'const' ( constSpec | '(' ( constSpec eos )* ')' )
    ;

//ConstSpec      = IdentifierList [ [ Type ] "=" ExpressionList ] .
constSpec
    : identifierList ( type? '=' expressionList )?
    ;

//
//IdentifierList = identifier { "," identifier } .
identifierList
    : IDENTIFIER ( ',' IDENTIFIER )*
    ;

//ExpressionList = Expression { "," Expression } .
expressionList
    : expression ( ',' expression )*
    ;

//TypeDecl     = "type" ( TypeSpec | "(" { TypeSpec ";" } ")" ) .
typeDecl
    : 'type' ( typeSpec | '(' ( typeSpec eos )* ')' )
    ;

//TypeSpec     = identifier Type .
typeSpec
    : IDENTIFIER type
    ;


// Function declarations

//FunctionDecl = "func" FunctionName ( Function | Signature ) .
//FunctionName = identifier .
//Function     = Signature FunctionBody .
//FunctionBody = Block .
functionDecl
    : 'func' IDENTIFIER ( function | signature )
    ;

function
    : signature block
    ;

//MethodDecl   = "func" Receiver MethodName ( Function | Signature ) .
//Receiver     = Parameters .
methodDecl
    : 'func' receiver IDENTIFIER ( function | signature )
    ;

receiver
    : parameters
    ;

//VarDecl     = "var" ( VarSpec | "(" { VarSpec ";" } ")" ) .
//VarSpec     = IdentifierList ( Type [ "=" ExpressionList ] | "=" ExpressionList ) .
varDecl
    : 'var' ( varSpec | '(' ( varSpec eos )* ')' )
    ;

varSpec
    : identifierList ( type ( '=' expressionList )? | '=' expressionList )
    ;


//Block = "{" StatementList "}" .
block
    : '{' statementList '}'
    ;

//StatementList = { Statement ";" } .
statementList
    : ( statement eos )*
    ;

statement
    : declaration
    | labeledStmt
    | simpleStmt
    | goStmt
    | returnStmt
    | breakStmt
    | continueStmt
    | gotoStmt
    | fallthroughStmt
    | block
    | ifStmt
    | switchStmt
    | selectStmt
    | forStmt
    | deferStmt
	;

//SimpleStmt = EmptyStmt | ExpressionStmt | SendStmt | IncDecStmt | Assignment | ShortVarDecl .
simpleStmt
    : sendStmt
    | expressionStmt
    | incDecStmt
    | assignment
    | shortVarDecl
    | emptyStmt
    ;

//ExpressionStmt = Expression .
expressionStmt
    : expression
    ;

//SendStmt = Channel "<-" Expression .
//Channel  = Expression .
sendStmt
    : expression '<-' expression
    ;

//IncDecStmt = Expression ( "++" | "--" ) .
incDecStmt
    : expression ( '++' | '--' )
    ;

//Assignment = ExpressionList assign_op ExpressionList .
assignment
    : expressionList assign_op expressionList
    ;

//assign_op = [ add_op | mul_op ] "=" .
assign_op
    : ('+' | '-' | '|' | '^' | '*' | '/' | '%' | '<<' | '>>' | '&' | '&^')? '='
    ;


//ShortVarDecl = IdentifierList ":=" ExpressionList .
shortVarDecl
    : identifierList ':=' expressionList
    ;

emptyStmt
    : ';'
    ;

//LabeledStmt = Label ":" Statement .
//Label       = identifier .
labeledStmt
    : IDENTIFIER ':' statement
    ;

//ReturnStmt = "return" [ ExpressionList ] .
returnStmt
    : 'return' expressionList?
    ;

//BreakStmt = "break" [ Label ] .
breakStmt
    : 'break' IDENTIFIER?
    ;

//ContinueStmt = "continue" [ Label ] .
continueStmt
    : 'continue' IDENTIFIER?
    ;

//GotoStmt = "goto" Label .
gotoStmt
    : 'goto' IDENTIFIER
    ;

//FallthroughStmt = "fallthrough" .
fallthroughStmt
    : 'fallthrough'
    ;

//DeferStmt = "defer" Expression .
deferStmt
    : 'defer' expression
    ;

//IfStmt = "if" [ SimpleStmt ";" ] Expression Block [ "else" ( IfStmt | Block ) ] .
ifStmt
    : 'if' (simpleStmt ';')? expression block ( 'else' ( ifStmt | block ) )?
    ;

//SwitchStmt = ExprSwitchStmt | TypeSwitchStmt .
switchStmt
    : exprSwitchStmt | typeSwitchStmt
    ;

//ExprSwitchStmt = "switch" [ SimpleStmt ";" ] [ Expression ] "{" { ExprCaseClause } "}" .
//ExprCaseClause = ExprSwitchCase ":" StatementList .
//ExprSwitchCase = "case" ExpressionList | "default" .
exprSwitchStmt
    : 'switch' ( simpleStmt ';' )? expression? '{' exprCaseClause* '}'
    ;

exprCaseClause
    : exprSwitchCase ':' statementList
    ;

exprSwitchCase
    : 'case' expressionList | 'default'
    ;

//TypeSwitchStmt  = "switch" [ SimpleStmt ";" ] TypeSwitchGuard "{" { TypeCaseClause } "}" .
//TypeSwitchGuard = [ identifier ":=" ] PrimaryExpr "." "(" "type" ")" .
//TypeCaseClause  = TypeSwitchCase ":" StatementList .
//TypeSwitchCase  = "case" TypeList | "default" .
//TypeList        = Type { "," Type } .
typeSwitchStmt
    : 'switch' ( simpleStmt ';' )? typeSwitchGuard '{' typeCaseClause* '}'
    ;
typeSwitchGuard
    : ( IDENTIFIER ':=' )? primaryExpr '.' '(' 'type' ')'
    ;
typeCaseClause
    : typeSwitchCase ':' statementList
    ;
typeSwitchCase
    : 'case' typeList | 'default'
    ;
typeList
    : type ( ',' type )*
    ;


//SelectStmt = "select" "{" { CommClause } "}" .
//CommClause = CommCase ":" StatementList .
//CommCase   = "case" ( SendStmt | RecvStmt ) | "default" .
//RecvStmt   = [ ExpressionList "=" | IdentifierList ":=" ] RecvExpr .
//RecvExpr   = Expression .
selectStmt
    : 'select' '{' commClause* '}'
    ;
commClause
    : commCase ':' statementList
    ;
commCase
    : 'case' ( sendStmt | recvStmt ) | 'default'
    ;
recvStmt
    : ( expressionList '=' | identifierList ':=' )? expression
    ;

//ForStmt = "for" [ Condition | ForClause | RangeClause ] Block .
//Condition = Expression .
forStmt
    : 'for' ( expression | forClause | rangeClause )? block
    ;

//ForClause = [ InitStmt ] ";" [ Condition ] ";" [ PostStmt ] .
//InitStmt = SimpleStmt .
//PostStmt = SimpleStmt .
forClause
    : simpleStmt? ';' expression? ';' simpleStmt?
    ;


//RangeClause = [ ExpressionList "=" | IdentifierList ":=" ] "range" Expression .
rangeClause
    : (expressionList '=' | identifierList ':=' )? 'range' expression
    ;

//GoStmt = "go" Expression .
goStmt
    : 'go' expression
    ;

//Type      = TypeName | TypeLit | "(" Type ")" .
type
    : typeName
    | typeLit
    | '(' type ')'
    ;

//TypeName  = identifier | QualifiedIdent .
typeName
    : IDENTIFIER
    | qualifiedIdent
    ;

//TypeLit   = ArrayType | StructType | PointerType | FunctionType | InterfaceType |
//	    SliceType | MapType | ChannelType .
typeLit
    : arrayType
    | structType
    | pointerType
    | functionType
    | interfaceType
    | sliceType
    | mapType
    | channelType
    ;


arrayType
    : '[' arrayLength ']' elementType
    ;

arrayLength
    : expression
    ;

elementType
    : type
    ;

//PointerType = "*" BaseType .
//BaseType    = Type .
pointerType
    : '*' type
    ;

//InterfaceType      = "interface" "{" { MethodSpec ";" } "}" .
//MethodSpec         = MethodName Signature | InterfaceTypeName .
//MethodName         = identifier .
//InterfaceTypeName  = TypeName .
interfaceType
    : 'interface' '{' ( methodSpec eos )* '}'
    ;

//SliceType = "[" "]" ElementType .
sliceType
    : '[' ']' elementType
    ;

//MapType     = "map" "[" KeyType "]" ElementType .
//KeyType     = Type .
mapType
    : 'map' '[' type ']' elementType
    ;

//ChannelType = ( "chan" | "chan" "<-" | "<-" "chan" ) ElementType .
channelType
    : ( 'chan' | 'chan' '<-' | '<-' 'chan' ) elementType
    ;

methodSpec
    : {noTerminatorAfterParams(2)}? IDENTIFIER parameters result
    | typeName
    | IDENTIFIER parameters
    ;


//FunctionType   = "func" Signature .
//Signature      = Parameters [ Result ] .
//Result         = Parameters | Type .
//Parameters     = "(" [ ParameterList [ "," ] ] ")" .
//ParameterList  = ParameterDecl { "," ParameterDecl } .
//ParameterDecl  = [ IdentifierList ] [ "..." ] Type .
functionType
    : 'func' signature
    ;

signature
    : {noTerminatorAfterParams(1)}? parameters result
    | parameters
    ;

result
    : parameters
    | type
    ;

parameters
    : '(' ( parameterList ','? )? ')'
    ;

parameterList
    : parameterDecl ( ',' parameterDecl )*
    ;

parameterDecl
    : identifierList? '...'? type
    ;

/////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Operands

//Operand     = Literal | OperandName | MethodExpr | "(" Expression ")" .
//Literal     = BasicLit | CompositeLit | FunctionLit .
//BasicLit    = int_lit | float_lit | imaginary_lit | rune_lit | string_lit .
//OperandName = identifier | QualifiedIdent.

operand
    : literal
    | operandName
    | methodExpr
    | '(' expression ')'
    ;

literal
    : basicLit
    | compositeLit
    | functionLit
    ;

basicLit
    : integer
    | FLOAT_LIT
    | IMAGINARY_LIT
    | RUNE_LIT
    | STRING_LIT
    ;

integer
    : INT_LIT
    | IMAGINARY_LIT
    | RUNE_LIT
    ;

operandName
    : IDENTIFIER
    | qualifiedIdent
    ;

//QualifiedIdent = PackageName "." identifier .
qualifiedIdent
    : IDENTIFIER '.' IDENTIFIER
    ;

//CompositeLit  = LiteralType LiteralValue .
//LiteralType   = StructType | ArrayType | "[" "..." "]" ElementType |
//                SliceType | MapType | TypeName .
//LiteralValue  = "{" [ ElementList [ "," ] ] "}" .
//ElementList   = KeyedElement { "," KeyedElement } .
//KeyedElement  = [ Key ":" ] Element .
//Key           = FieldName | Expression | LiteralValue .
//FieldName     = identifier .
//Element       = Expression | LiteralValue .

compositeLit
    : literalType literalValue
    ;

literalType
    : structType
    | arrayType
    | '[' '...' ']' elementType
    | sliceType
    | mapType
    | typeName
    ;

literalValue
    : '{' ( elementList ','? )? '}'
    ;

elementList
    : keyedElement (',' keyedElement)*
    ;

keyedElement
    : (key ':')? element
    ;

key
    : IDENTIFIER
    | expression
    | literalValue
    ;

element
    : expression
    | literalValue
    ;

//StructType     = "struct" "{" { FieldDecl ";" } "}" .
//FieldDecl      = (IdentifierList Type | AnonymousField) [ Tag ] .
//AnonymousField = [ "*" ] TypeName .
//Tag            = string_lit .
structType
    : 'struct' '{' ( fieldDecl eos )* '}'
    ;

fieldDecl
    : ({noTerminatorBetween(2)}? identifierList type | anonymousField) STRING_LIT?
    ;

anonymousField
    : '*'? typeName
    ;

//FunctionLit = "func" Function .
functionLit
    : 'func' function
    ;

//PrimaryExpr =
//	Operand |
//	Conversion |
//	PrimaryExpr Selector |
//	PrimaryExpr Index |
//	PrimaryExpr Slice |
//	PrimaryExpr TypeAssertion |
//	PrimaryExpr Arguments .
//
//Selector       = "." identifier .
//Index          = "[" Expression "]" .
//Slice          = "[" ( [ Expression ] ":" [ Expression ] ) |
//                     ( [ Expression ] ":" Expression ":" Expression )
//                 "]" .
//TypeAssertion  = "." "(" Type ")" .
//Arguments      = "(" [ ( ExpressionList | Type [ "," ExpressionList ] ) [ "..." ] [ "," ] ] ")" .

primaryExpr
    : operand
    | conversion
    | primaryExpr selector
    | primaryExpr index
    | primaryExpr slice
    | primaryExpr typeAssertion
	| primaryExpr arguments
    ;

selector
    : '.' IDENTIFIER
    ;

index
    : '[' expression ']'
    ;

slice
    : '[' (( expression? ':' expression? ) | ( expression? ':' expression ':' expression )) ']'
    ;

typeAssertion
    : '.' '(' type ')'
    ;

arguments
    : '(' ( ( expressionList | type ( ',' expressionList )? ) '...'? ','? )? ')'
    ;

//MethodExpr    = ReceiverType "." MethodName .
//ReceiverType  = TypeName | "(" "*" TypeName ")" | "(" ReceiverType ")" .
methodExpr
    : receiverType '.' IDENTIFIER
    ;

receiverType
    : typeName
    | '(' '*' typeName ')'
    | '(' receiverType ')'
    ;

//Expression = UnaryExpr | Expression binary_op Expression .
//UnaryExpr  = PrimaryExpr | unary_op UnaryExpr .

expression
    : unaryExpr
//    | expression BINARY_OP expression
    | expression ('||' | '&&' | '==' | '!=' | '<' | '<=' | '>' | '>=' | '+' | '-' | '|' | '^' | '*' | '/' | '%' | '<<' | '>>' | '&' | '&^') expression
    ;

unaryExpr
    : primaryExpr
    | ('+'|'-'|'!'|'^'|'*'|'&'|'<-') unaryExpr
    ;

//Conversion = Type "(" Expression [ "," ] ")" .
conversion
    : type '(' expression ','? ')'
    ;

eos
    : ';'
    | EOF
    | {lineTerminatorAhead()}?
    | {_input.LT(1).getText().equals("}") }?
    ;

