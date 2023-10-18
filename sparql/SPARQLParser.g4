/*
 * Copyright (c) 2023 by Bart Kiers
 *
 * The MIT license.
 *
 * Permission is hereby granted, free of charge, to any person
 * obtaining a copy of this software and associated documentation
 * files (the "Software"), to deal in the Software without
 * restriction, including without limitation the rights to use,
 * copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the
 * Software is furnished to do so, subject to the following
 * conditions:
 *
 * The above copyright notice and this permission notice shall be
 * included in all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
 * EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES
 * OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
 * NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
 * HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
 * WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
 * FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
 * OTHER DEALINGS IN THE SOFTWARE.
 *
 * (REVISION Last updated: 18 October 2023)
 */
/// Grammar according the latest (October 2023) specs: https://www.w3.org/TR/sparql11-query/#grammar
parser grammar SPARQLParser;

options {
  tokenVocab=SPARQLLexer;
}

///  [1]    QueryUnit : Query
queryUnit : query EOF;

///  [3]    UpdateUnit : Update
updateUnit : update EOF;

///  [2]    Query : Prologue ( SelectQuery | ConstructQuery | DescribeQuery | AskQuery ) ValuesClause
query : prologue ( selectQuery | constructQuery | describeQuery | askQuery ) valuesClause;

///  [4]    Prologue : ( BaseDecl | PrefixDecl )*
prologue : ( baseDecl | prefixDecl )*;

///  [5]    BaseDecl : 'BASE' IRIREF
baseDecl : BASE IRIREF;

///  [6]    PrefixDecl : 'PREFIX' PNAME_NS IRIREF
prefixDecl : PREFIX PNAME_NS IRIREF;

///  [7]    SelectQuery : SelectClause DatasetClause* WhereClause SolutionModifier
selectQuery : selectClause datasetClause* whereClause solutionModifier;

///  [8]    SubSelect : SelectClause WhereClause SolutionModifier ValuesClause
subSelect : selectClause whereClause solutionModifier valuesClause;

///  [9]    SelectClause : 'SELECT' ( 'DISTINCT' | 'REDUCED' )? ( ( Var | ( '(' Expression 'AS' Var ')' ) )+ | '*' )
selectClause : SELECT ( DISTINCT | REDUCED )? ( ( var | ( OPAR expression AS var CPAR ) )+ | MUL );

/// [10]    ConstructQuery : 'CONSTRUCT' ( ConstructTemplate DatasetClause* WhereClause SolutionModifier | DatasetClause* 'WHERE' '{' TriplesTemplate? '}' SolutionModifier )
constructQuery
 : CONSTRUCT ( constructTemplate datasetClause* whereClause solutionModifier
             | datasetClause* WHERE OBRACE triplesTemplate? CBRACE solutionModifier
             )
 ;

/// [11]    DescribeQuery : 'DESCRIBE' ( VarOrIri+ | '*' ) DatasetClause* WhereClause? SolutionModifier
describeQuery : DESCRIBE ( varOrIri+ | MUL ) datasetClause* whereClause? solutionModifier;

/// [12]    AskQuery : 'ASK' DatasetClause* WhereClause SolutionModifier
askQuery : ASK datasetClause* whereClause solutionModifier;

/// [13]    DatasetClause : 'FROM' ( DefaultGraphClause | NamedGraphClause )
datasetClause : FROM ( defaultGraphClause | namedGraphClause );

/// [14]    DefaultGraphClause : SourceSelector
defaultGraphClause : sourceSelector;

/// [15]    NamedGraphClause : 'NAMED' SourceSelector
namedGraphClause : NAMED sourceSelector;

/// [16]    SourceSelector : iri
sourceSelector : iri;

/// [17]    WhereClause : 'WHERE'? GroupGraphPattern
whereClause : WHERE? groupGraphPattern;

/// [18]    SolutionModifier : GroupClause? HavingClause? OrderClause? LimitOffsetClauses?
solutionModifier : groupClause? havingClause? orderClause? limitOffsetClauses?;

/// [19]    GroupClause : 'GROUP' 'BY' GroupCondition+
groupClause : GROUP BY groupCondition+;

/// [20]    GroupCondition : BuiltInCall | FunctionCall | '(' Expression ( 'AS' Var )? ')' | Var
groupCondition
 : builtInCall
 | functionCall
 | OPAR expression ( AS var )? CPAR
 | var
 ;

/// [21]    HavingClause : 'HAVING' HavingCondition+
havingClause : HAVING havingCondition+;

/// [22]    HavingCondition : Constraint
havingCondition : constraint;

/// [23]    OrderClause : 'ORDER' 'BY' OrderCondition+
orderClause : ORDER BY orderCondition+;

/// [24]    OrderCondition
///          : ( ( 'ASC' | 'DESC' ) BrackettedExpression )
///          | ( Constraint | Var )
orderCondition
 : ( ASC | DESC ) brackettedExpression
 | constraint
 | var
 ;

/// [25]    LimitOffsetClauses : LimitClause OffsetClause? | OffsetClause LimitClause?
limitOffsetClauses
 : limitClause offsetClause?
 | offsetClause limitClause?
 ;

/// [26]    LimitClause : 'LIMIT' INTEGER
limitClause : LIMIT INTEGER;

/// [27]    OffsetClause : 'OFFSET' INTEGER
offsetClause : OFFSET INTEGER;

/// [28]    ValuesClause : ( 'VALUES' DataBlock )?
valuesClause : ( VALUES dataBlock )?;

/// [29]    Update : Prologue ( Update1 ( ';' Update )? )?
update : prologue ( update1 ( SCOL update )? )?;

/// [30]    Update1 : Load | Clear | Drop | Add | Move | Copy | Create | InsertData | DeleteData | DeleteWhere | Modify
update1
 : load
 | clear
 | drop
 | add
 | move
 | copy
 | create
 | insertData
 | deleteData
 | deleteWhere
 | modify
 ;

/// [31]    Load : 'LOAD' 'SILENT'? iri ( 'INTO' GraphRef )?
load : LOAD SILENT? iri ( INTO graphRef )?;

/// [32]    Clear : 'CLEAR' 'SILENT'? GraphRefAll
clear : CLEAR SILENT? graphRefAll;

/// [33]    Drop : 'DROP' 'SILENT'? GraphRefAll
drop : DROP SILENT? graphRefAll;

/// [34]    Create : 'CREATE' 'SILENT'? GraphRef
create : CREATE SILENT? graphRef;

/// [35]    Add : 'ADD' 'SILENT'? GraphOrDefault 'TO' GraphOrDefault
add : ADD SILENT? graphOrDefault TO graphOrDefault;

/// [36]    Move : 'MOVE' 'SILENT'? GraphOrDefault 'TO' GraphOrDefault
move : MOVE SILENT? graphOrDefault TO graphOrDefault;

/// [37]    Copy : 'COPY' 'SILENT'? GraphOrDefault 'TO' GraphOrDefault
copy : COPY SILENT? graphOrDefault TO graphOrDefault;

/// [38]    InsertData : 'INSERT DATA' QuadData
insertData : INSERT DATA quadData;

/// [39]    DeleteData : 'DELETE DATA' QuadData
deleteData : DELETE DATA quadData;

/// [40]    DeleteWhere : 'DELETE WHERE' QuadPattern
deleteWhere : DELETE WHERE quadPattern;

/// [41]    Modify : ( 'WITH' iri )? ( DeleteClause InsertClause? | InsertClause ) UsingClause* 'WHERE' GroupGraphPattern
modify : ( WITH iri )? ( deleteClause insertClause? | insertClause ) usingClause* WHERE groupGraphPattern;

/// [42]    DeleteClause : 'DELETE' QuadPattern
deleteClause : DELETE quadPattern;

/// [43]    InsertClause : 'INSERT' QuadPattern
insertClause : INSERT quadPattern;

/// [44]    UsingClause : 'USING' ( iri | 'NAMED' iri )
usingClause : USING ( iri | NAMED iri );

/// [45]    GraphOrDefault : 'DEFAULT' | 'GRAPH'? iri
graphOrDefault
 : DEFAULT
 | GRAPH? iri
 ;

/// [46]    GraphRef : 'GRAPH' iri
graphRef : GRAPH iri;

/// [47]    GraphRefAll : GraphRef | 'DEFAULT' | 'NAMED' | 'ALL'
graphRefAll
 : graphRef
 | DEFAULT
 | NAMED
 | ALL
 ;

/// [48]    QuadPattern : '{' Quads '}'
quadPattern : OBRACE quads CBRACE;

/// [49]    QuadData : '{' Quads '}'
quadData : OBRACE quads CBRACE;

/// [50]    Quads : TriplesTemplate? ( QuadsNotTriples '.'? TriplesTemplate? )*
quads : triplesTemplate? ( quadsNotTriples DOT? triplesTemplate? )*;

/// [51]    QuadsNotTriples : 'GRAPH' VarOrIri '{' TriplesTemplate? '}'
quadsNotTriples : GRAPH varOrIri OBRACE triplesTemplate? CBRACE;

/// [52]    TriplesTemplate : TriplesSameSubject ( '.' TriplesTemplate? )?
triplesTemplate : triplesSameSubject ( DOT triplesTemplate? )?;

/// [53]    GroupGraphPattern : '{' ( SubSelect | GroupGraphPatternSub ) '}'
groupGraphPattern : OBRACE ( subSelect | groupGraphPatternSub ) CBRACE;

/// [54]    GroupGraphPatternSub : TriplesBlock? ( GraphPatternNotTriples '.'? TriplesBlock? )*
groupGraphPatternSub : triplesBlock? ( graphPatternNotTriples DOT? triplesBlock? )*;

/// [55]    TriplesBlock : TriplesSameSubjectPath ( '.' TriplesBlock? )?
triplesBlock : triplesSameSubjectPath ( DOT triplesBlock? )?;

/// [56]    GraphPatternNotTriples : GroupOrUnionGraphPattern | OptionalGraphPattern | MinusGraphPattern | GraphGraphPattern | ServiceGraphPattern | Filter | Bind | InlineData
graphPatternNotTriples
 : groupOrUnionGraphPattern
 | optionalGraphPattern
 | minusGraphPattern
 | graphGraphPattern
 | serviceGraphPattern
 | filter
 | bind
 | inlineData
 ;

/// [57]    OptionalGraphPattern : 'OPTIONAL' GroupGraphPattern
optionalGraphPattern : OPTIONAL groupGraphPattern;

/// [58]    GraphGraphPattern : 'GRAPH' VarOrIri GroupGraphPattern
graphGraphPattern : GRAPH varOrIri groupGraphPattern;

/// [59]    ServiceGraphPattern : 'SERVICE' 'SILENT'? VarOrIri GroupGraphPattern
serviceGraphPattern : SERVICE SILENT? varOrIri groupGraphPattern;

/// [60]    Bind : 'BIND' '(' Expression 'AS' Var ')'
bind : BIND OPAR expression AS var CPAR;

/// [61]    InlineData : 'VALUES' DataBlock
inlineData : VALUES dataBlock;

/// [62]    DataBlock : InlineDataOneVar | InlineDataFull
dataBlock : inlineDataOneVar | inlineDataFull;

/// [63]    InlineDataOneVar : Var '{' DataBlockValue* '}'
inlineDataOneVar : var OBRACE dataBlockValue* CBRACE;

/// [64]    InlineDataFull : ( NIL | '(' Var* ')' ) '{' ( '(' DataBlockValue* ')' | NIL )* '}'
inlineDataFull : ( NIL | OPAR var* CPAR ) OBRACE ( OPAR dataBlockValue* CPAR | NIL )* CBRACE;

/// [65]    DataBlockValue : iri | RDFLiteral | NumericLiteral | BooleanLiteral | 'UNDEF'
dataBlockValue : iri | rdfLiteral | numericLiteral | booleanLiteral | UNDEF;

/// [66]    MinusGraphPattern : 'MINUS' GroupGraphPattern
minusGraphPattern : MINUS groupGraphPattern;

/// [67]    GroupOrUnionGraphPattern : GroupGraphPattern ( 'UNION' GroupGraphPattern )*
groupOrUnionGraphPattern : groupGraphPattern ( UNION groupGraphPattern )*;

/// [68]    Filter : 'FILTER' Constraint
filter : FILTER constraint;

/// [69]    Constraint : BrackettedExpression | BuiltInCall | FunctionCall
constraint
 : brackettedExpression
 | builtInCall
 | functionCall
 ;

/// [70]    FunctionCall : iri ArgList
functionCall : iri argList;

/// [71]    ArgList : NIL | '(' 'DISTINCT'? Expression ( ',' Expression )* ')'
argList
 : NIL
 | OPAR DISTINCT? expression ( COMMA expression )* CPAR
 ;

/// [72]    ExpressionList : NIL | '(' Expression ( ',' Expression )* ')'
expressionList
 : NIL
 | OPAR expression ( COMMA expression )* CPAR
 ;

/// [73]    ConstructTemplate : '{' ConstructTriples? '}'
constructTemplate : OBRACE constructTriples? CBRACE;

/// [74]    ConstructTriples : TriplesSameSubject ( '.' ConstructTriples? )?
constructTriples : triplesSameSubject ( DOT constructTriples? )?;

/// [75]    TriplesSameSubject : VarOrTerm PropertyListNotEmpty | TriplesNode PropertyList
triplesSameSubject
 : varOrTerm propertyListNotEmpty
 | triplesNode propertyList
 ;

/// [76]    PropertyList : PropertyListNotEmpty?
propertyList : propertyListNotEmpty?;

/// [77]    PropertyListNotEmpty : Verb ObjectList ( ';' ( Verb ObjectList )? )*
propertyListNotEmpty : verb objectList ( SCOL ( verb objectList )? )*;

/// [78]    Verb : VarOrIri | 'a'
verb
 : varOrIri
 | A_
 ;

/// [79]    ObjectList : Object ( ',' Object )*
objectList : object ( COMMA object )*;

/// [80]    Object : GraphNode
object : graphNode;

/// [81]    TriplesSameSubjectPath : VarOrTerm PropertyListPathNotEmpty | TriplesNodePath PropertyListPath
triplesSameSubjectPath
 : varOrTerm propertyListPathNotEmpty
 | triplesNodePath propertyListPath
 ;

/// [82]    PropertyListPath : PropertyListPathNotEmpty?
propertyListPath : propertyListPathNotEmpty?;

/// [83]    PropertyListPathNotEmpty : ( VerbPath | VerbSimple ) ObjectListPath ( ';' ( ( VerbPath | VerbSimple ) ObjectList )? )*
propertyListPathNotEmpty
 : ( verbPath | verbSimple ) objectListPath ( SCOL ( ( verbPath | verbSimple ) objectList )? )*
 ;

/// [84]    VerbPath : Path
verbPath : path;

/// [85]    VerbSimple : Var
verbSimple : var;

/// [86]    ObjectListPath : ObjectPath ( ',' ObjectPath )*
objectListPath : objectPath ( COMMA objectPath )*;

/// [87]    ObjectPath : GraphNodePath
objectPath : graphNodePath;

/// [88]    Path : PathAlternative
path : pathAlternative;

/// [89]    PathAlternative : PathSequence ( '|' PathSequence )*
pathAlternative : pathSequence ( PIPE pathSequence )*;

/// [90]    PathSequence : PathEltOrInverse ( '/' PathEltOrInverse )*
pathSequence : pathEltOrInverse ( DIV pathEltOrInverse )*;

/// [91]    PathElt : PathPrimary PathMod?
pathElt : pathPrimary pathMod?;

/// [92]    PathEltOrInverse : PathElt | '^' PathElt
pathEltOrInverse
 : pathElt
 | CARET pathElt
 ;

/// [93]    PathMod : '?' | '*' | '+'
pathMod
 : QMARK
 | MUL
 | ADD
 ;

/// [94]    PathPrimary : iri | 'a' | '!' PathNegatedPropertySet | '(' Path ')'
pathPrimary
 : iri
 | A_
 | EXCL pathNegatedPropertySet
 | OPAR path CPAR
 ;

/// [95]    PathNegatedPropertySet : PathOneInPropertySet | '(' ( PathOneInPropertySet ( '|' PathOneInPropertySet )* )? ')'
pathNegatedPropertySet
 : pathOneInPropertySet
 | OPAR ( pathOneInPropertySet ( PIPE pathOneInPropertySet )* )? CPAR
 ;

/// [96]    PathOneInPropertySet : iri | 'a' | '^' ( iri | 'a' )
pathOneInPropertySet
 : iri
 | A_
 | CARET ( iri | A_ )
 ;

/// [98]    TriplesNode : Collection | BlankNodePropertyList
triplesNode
 : collection
 | blankNodePropertyList
 ;

/// [99]    BlankNodePropertyList : '[' PropertyListNotEmpty ']'
blankNodePropertyList : OBRACK propertyListNotEmpty CBRACK;

/// [100]    TriplesNodePath : CollectionPath | BlankNodePropertyListPath
triplesNodePath : collectionPath | blankNodePropertyListPath;

/// [101]    BlankNodePropertyListPath : '[' PropertyListPathNotEmpty ']'
blankNodePropertyListPath : OBRACK propertyListPathNotEmpty CBRACK;

/// [102]    Collection : '(' GraphNode+ ')'
collection : OPAR graphNode+ CPAR;

/// [103]    CollectionPath : '(' GraphNodePath+ ')'
collectionPath : OPAR graphNodePath+ CPAR;

/// [104]    GraphNode : VarOrTerm | TriplesNode
graphNode : varOrTerm | triplesNode;

/// [105]    GraphNodePath : VarOrTerm | TriplesNodePath
graphNodePath : varOrTerm | triplesNodePath;

/// [106]    VarOrTerm : Var | GraphTerm
varOrTerm : var | graphTerm;

/// [107]    VarOrIri : Var | iri
varOrIri : var | iri;

/// [108]    Var : VAR1 | VAR2
var : VAR1 | VAR2;

/// [109]    GraphTerm : iri | RDFLiteral | NumericLiteral | BooleanLiteral | BlankNode | NIL
graphTerm : iri | rdfLiteral | numericLiteral | booleanLiteral | blankNode | NIL;

/// [110]    Expression : ConditionalOrExpression
expression : conditionalOrExpression;

/// [111]    ConditionalOrExpression : ConditionalAndExpression ( '||' ConditionalAndExpression )*
conditionalOrExpression : conditionalAndExpression ( OR conditionalAndExpression )*;

/// [112]    ConditionalAndExpression : ValueLogical ( '&&' ValueLogical )*
conditionalAndExpression : valueLogical ( AND valueLogical )*;

/// [113]    ValueLogical : RelationalExpression
valueLogical : relationalExpression;

/// [114]    RelationalExpression : NumericExpression ( '=' NumericExpression | '!=' NumericExpression | '<' NumericExpression | '>' NumericExpression | '<=' NumericExpression | '>=' NumericExpression | 'IN' ExpressionList | 'NOT' 'IN' ExpressionList )?
relationalExpression
 : numericExpression ( EQ numericExpression
                     | NEQ numericExpression
                     | LT numericExpression
                     | GT numericExpression
                     | LTE numericExpression
                     | GTE numericExpression
                     | IN expressionList
                     | NOT IN expressionList
                     )?;

/// [115]    NumericExpression : AdditiveExpression
numericExpression : additiveExpression;

/// [116]    AdditiveExpression : MultiplicativeExpression ( '+' MultiplicativeExpression | '-' MultiplicativeExpression | ( NumericLiteralPositive | NumericLiteralNegative ) ( ( '*' UnaryExpression ) | ( '/' UnaryExpression ) )* )*
additiveExpression
 : multiplicativeExpression ( ADD multiplicativeExpression
                            | SUB multiplicativeExpression
                            | ( numericLiteralPositive | numericLiteralNegative )
                              ( ( MUL unaryExpression )
                              | ( DIV unaryExpression )
                              )*
                            )*;

/// [117]    MultiplicativeExpression : UnaryExpression ( '*' UnaryExpression | '/' UnaryExpression )*
multiplicativeExpression : unaryExpression ( MUL unaryExpression | DIV unaryExpression )*;

/// [118]    UnaryExpression
///           : '!' PrimaryExpression
///           | '+' PrimaryExpression
///           | '-' PrimaryExpression
///           | PrimaryExpression
unaryExpression
 : EXCL primaryExpression
 | ADD primaryExpression
 | SUB primaryExpression
 | primaryExpression
 ;

/// [119]    PrimaryExpression : BrackettedExpression | BuiltInCall | iriOrFunction | RDFLiteral | NumericLiteral | BooleanLiteral | Var
primaryExpression
 : brackettedExpression
 | builtInCall
 | iriOrFunction
 | rdfLiteral
 | numericLiteral
 | booleanLiteral
 | var
 ;

/// [120]    BrackettedExpression : '(' Expression ')'
brackettedExpression : OPAR expression CPAR;

/// [121]    BuiltInCall
///           : Aggregate
///           | 'STR' '(' Expression ')'
///           | 'LANG' '(' Expression ')'
///           | 'LANGMATCHES' '(' Expression ',' Expression ')'
///           | 'DATATYPE' '(' Expression ')'
///           | 'BOUND' '(' Var ')'
///           | 'IRI' '(' Expression ')'
///           | 'URI' '(' Expression ')'
///           | 'BNODE' ( '(' Expression ')' | NIL )
///           | 'RAND' NIL
///           | 'ABS' '(' Expression ')'
///           | 'CEIL' '(' Expression ')'
///           | 'FLOOR' '(' Expression ')'
///           | 'ROUND' '(' Expression ')'
///           | 'CONCAT' ExpressionList
///           | SubstringExpression
///           | 'STRLEN' '(' Expression ')'
///           | StrReplaceExpression
///           | 'UCASE' '(' Expression ')'
///           | 'LCASE' '(' Expression ')'
///           | 'ENCODE_FOR_URI' '(' Expression ')'
///           | 'CONTAINS' '(' Expression ',' Expression ')'
///           | 'STRSTARTS' '(' Expression ',' Expression ')'
///           | 'STRENDS' '(' Expression ',' Expression ')'
///           | 'STRBEFORE' '(' Expression ',' Expression ')'
///           | 'STRAFTER' '(' Expression ',' Expression ')'
///           | 'YEAR' '(' Expression ')'
///           | 'MONTH' '(' Expression ')'
///           | 'DAY' '(' Expression ')'
///           | 'HOURS' '(' Expression ')'
///           | 'MINUTES' '(' Expression ')'
///           | 'SECONDS' '(' Expression ')'
///           | 'TIMEZONE' '(' Expression ')'
///           | 'TZ' '(' Expression ')'
///           | 'NOW' NIL
///           | 'UUID' NIL
///           | 'STRUUID' NIL
///           | 'MD5' '(' Expression ')'
///           | 'SHA1' '(' Expression ')'
///           | 'SHA256' '(' Expression ')'
///           | 'SHA384' '(' Expression ')'
///           | 'SHA512' '(' Expression ')'
///           | 'COALESCE' ExpressionList
///           | 'IF' '(' Expression ',' Expression ',' Expression ')'
///           | 'STRLANG' '(' Expression ',' Expression ')'
///           | 'STRDT' '(' Expression ',' Expression ')'
///           | 'sameTerm' '(' Expression ',' Expression ')'
///           | 'isIRI' '(' Expression ')'
///           | 'isURI' '(' Expression ')'
///           | 'isBLANK' '(' Expression ')'
///           | 'isLITERAL' '(' Expression ')'
///           | 'isNUMERIC' '(' Expression ')'
///           | RegexExpression
///           | ExistsFunc
///           | NotExistsFunc
builtInCall
 : aggregate
 | STR OPAR expression CPAR
 | LANG OPAR expression CPAR
 | LANGMATCHES OPAR expression COMMA expression CPAR
 | DATATYPE OPAR expression CPAR
 | BOUND OPAR var CPAR
 | IRI OPAR expression CPAR
 | URI OPAR expression CPAR
 | BNODE ( OPAR expression CPAR | NIL )
 | RAND NIL
 | ABS OPAR expression CPAR
 | CEIL OPAR expression CPAR
 | FLOOR OPAR expression CPAR
 | ROUND OPAR expression CPAR
 | CONCAT expressionList
 | substringExpression
 | STRLEN OPAR expression CPAR
 | strReplaceExpression
 | UCASE OPAR expression CPAR
 | LCASE OPAR expression CPAR
 | ENCODE_FOR_URI OPAR expression CPAR
 | CONTAINS OPAR expression COMMA expression CPAR
 | STRSTARTS OPAR expression COMMA expression CPAR
 | STRENDS OPAR expression COMMA expression CPAR
 | STRBEFORE OPAR expression COMMA expression CPAR
 | STRAFTER OPAR expression COMMA expression CPAR
 | YEAR OPAR expression CPAR
 | MONTH OPAR expression CPAR
 | DAY OPAR expression CPAR
 | HOURS OPAR expression CPAR
 | MINUTES OPAR expression CPAR
 | SECONDS OPAR expression CPAR
 | TIMEZONE OPAR expression CPAR
 | TZ OPAR expression CPAR
 | NOW NIL
 | UUID NIL
 | STRUUID NIL
 | MD5 OPAR expression CPAR
 | SHA1 OPAR expression CPAR
 | SHA256 OPAR expression CPAR
 | SHA384 OPAR expression CPAR
 | SHA512 OPAR expression CPAR
 | COALESCE expressionList
 | IF OPAR expression COMMA expression COMMA expression CPAR
 | STRLANG OPAR expression COMMA expression CPAR
 | STRDT OPAR expression COMMA expression CPAR
 | SameTerm OPAR expression COMMA expression CPAR
 | IsIRI OPAR expression CPAR
 | IsURI OPAR expression CPAR
 | IsBLANK OPAR expression CPAR
 | IsLITERAL OPAR expression CPAR
 | IsNUMERIC OPAR expression CPAR
 | regexExpression
 | existsFunc
 | notExistsFunc
 ;

/// [122]    RegexExpression : 'REGEX' '(' Expression ',' Expression ( ',' Expression )? ')'
regexExpression : REGEX OPAR expression COMMA expression ( COMMA expression )? CPAR;

/// [123]    SubstringExpression : 'SUBSTR' '(' Expression ',' Expression ( ',' Expression )? ')'
substringExpression : SUBSTR OPAR expression COMMA expression ( COMMA expression )? CPAR;

/// [124]    StrReplaceExpression : 'REPLACE' '(' Expression ',' Expression ',' Expression ( ',' Expression )? ')'
strReplaceExpression : REPLACE OPAR expression COMMA expression COMMA expression ( COMMA expression )? CPAR;

/// [125]    ExistsFunc : 'EXISTS' GroupGraphPattern
existsFunc : EXISTS groupGraphPattern;

/// [126]    NotExistsFunc : 'NOT' 'EXISTS' GroupGraphPattern
notExistsFunc : NOT EXISTS groupGraphPattern;

/// [127]    Aggregate
///           : 'COUNT' '(' 'DISTINCT'? ( '*' | Expression ) ')'
///           | 'SUM' '(' 'DISTINCT'? Expression ')'
///           | 'MIN' '(' 'DISTINCT'? Expression ')'
///           | 'MAX' '(' 'DISTINCT'? Expression ')'
///           | 'AVG' '(' 'DISTINCT'? Expression ')'
///           | 'SAMPLE' '(' 'DISTINCT'? Expression ')'
///           | 'GROUP_CONCAT' '(' 'DISTINCT'? Expression ( ';' 'SEPARATOR' '=' String )? ')'
aggregate
 : COUNT OPAR DISTINCT? ( MUL | expression ) CPAR
 | SUM OPAR DISTINCT? expression CPAR
 | MIN OPAR DISTINCT? expression CPAR
 | MAX OPAR DISTINCT? expression CPAR
 | AVG OPAR DISTINCT? expression CPAR
 | SAMPLE OPAR DISTINCT? expression CPAR
 | GROUP_CONCAT OPAR DISTINCT? expression ( SCOL SEPARATOR EQ string )? CPAR
 ;

/// [128]    iriOrFunction : iri ArgList?
iriOrFunction : iri argList?;

/// [129]    RDFLiteral : String ( LANGTAG | ( '^^' iri ) )?
rdfLiteral : string ( LANGTAG | ( CARET2 iri ) )?;

/// [130]    NumericLiteral : NumericLiteralUnsigned | NumericLiteralPositive | NumericLiteralNegative
numericLiteral : numericLiteralUnsigned | numericLiteralPositive | numericLiteralNegative;

/// [131]    NumericLiteralUnsigned : INTEGER | DECIMAL | DOUBLE
numericLiteralUnsigned : INTEGER | DECIMAL | DOUBLE;

/// [132]    NumericLiteralPositive : INTEGER_POSITIVE | DECIMAL_POSITIVE | DOUBLE_POSITIVE
numericLiteralPositive : INTEGER_POSITIVE | DECIMAL_POSITIVE | DOUBLE_POSITIVE;

/// [133]    NumericLiteralNegative : INTEGER_NEGATIVE | DECIMAL_NEGATIVE | DOUBLE_NEGATIVE
numericLiteralNegative : INTEGER_NEGATIVE | DECIMAL_NEGATIVE | DOUBLE_NEGATIVE;

/// [134]    BooleanLiteral : 'true' | 'false'
booleanLiteral : TRUE | FALSE;

/// [135]    String : STRING_LITERAL1 | STRING_LITERAL2 | STRING_LITERAL_LONG1 | STRING_LITERAL_LONG2
string : STRING_LITERAL1 | STRING_LITERAL2 | STRING_LITERAL_LONG1 | STRING_LITERAL_LONG2;

/// [136]    iri : IRIREF | PrefixedName
iri : IRIREF | prefixedName;

/// [137]    PrefixedName : PNAME_LN | PNAME_NS
prefixedName : PNAME_LN | PNAME_NS;

/// [138]    BlankNode : BLANK_NODE_LABEL | ANON
blankNode : BLANK_NODE_LABEL | ANON;
