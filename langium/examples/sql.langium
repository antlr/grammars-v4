grammar Sql

entry SqlFile:
    statements+=Statement*
;

Statement:
    (DefinitionRule | RootLevelSelectStatement) ';'?;

DefinitionRule:
    CatalogDefinition | SchemaDefinition | TableDefinition | FunctionDefinition;

type Definition = CatalogDefinition | SchemaDefinition | TableLike | FunctionDefinition;

type ReferenceDefinition = SchemaDefinition | TableDefinition | FunctionDefinition;

//DEFINITION/////////////////////////////////////////////////////////

/**
 * A catalog is the top most layer inside of a database. In most sql dialects, it's expressed as
 * 
 * ```sql
 * CREATE DATABASE X;
 * ```
 * 
 * Some dialects use the `CATALOG` keyword instead to initialize a catalog.
 */
CatalogDefinition:
    'CREATE' (catalogSyntax?='CATALOG' | 'DATABASE') name=Identifier;

SchemaDefinition:
    'CREATE' 'SCHEMA' ReferenceNamed;

TableDefinition:
    'CREATE' 'TABLE' ReferenceNamed '('(
        columns+=TableContentDefinition (',' columns+=TableContentDefinition)* trailingComma?=','?
    )?')';

fragment ReferenceNamed:
    reference=GlobalReference;

GlobalReference:
    element=[Definition:Identifier] ({infer GlobalReference.previous=current} '.' element=[Definition:Identifier])*;

TableContentDefinition:
    ColumnDefinition
    | PrimaryKeyDefinition
    | IndexDefinition
    | KeyDefinition
    | ConstraintDefinition
;

ColumnDefinition:
    name=Identifier dataType=Type (negated?='NOT'? null?='NULL')?;

PrimaryKeyDefinition:
    'PRIMARY' 'KEY' '(' primaryKeys+=[ColumnDefinition] (',' primaryKeys+=[ColumnDefinition])* ')'
;

IndexDefinition:
    'INDEX' '(' indexes+=[ColumnDefinition] (',' indexes+=[ColumnDefinition])* ')'
;

KeyDefinition:
    unique?='UNIQUE'? 'KEY' name=Identifier '(' keys+=[ColumnDefinition] (',' keys+=[ColumnDefinition])* ')'
;

ConstraintDefinition:
    'CONSTRAINT' name=Identifier 'FOREIGN' 'KEY' '(' from=[ColumnDefinition] ')' 'REFERENCES' table=GlobalReference '(' to=[ColumnDefinition] ')'
    ('ON' 'DELETE' cascadeDeletion?='CASCADE')?
;

FunctionDefinition:
    'CREATE' ('OR' replace?='REPLACE')? 'FUNCTION' ReferenceNamed '('
        (params+=FormalParameterDefinition (',' params+=FormalParameterDefinition)*)?
    ')' 'AS' returnType=Type;

FormalParameterDefinition:
    name=Identifier dataType=Type
;
//END DEFINITION/////////////////////////////////////////////////////////

RootLevelSelectStatement:
    with=WithClause?
    select=SelectStatementRootExpression
;

SelectStatementRootExpression infers SelectTableExpression:
    SelectStatementUnionExpression
;

SelectStatementUnionExpression infers SelectTableExpression:
    SelectStatementIntersectExpression ({infer BinaryTableExpression.left=current} operator=UnionSetOperator right=SelectStatementIntersectExpression)*
;

SelectStatementIntersectExpression infers SelectTableExpression:
    SelectStatementPrimaryExpression ({infer BinaryTableExpression.left=current} operator=IntersectSetOperator right=SelectStatementPrimaryExpression)*
;

SelectStatementPrimaryExpression infers SelectTableExpression:
    {infer SimpleSelectTableExpression} select=SimpleSelectStatement
    | {infer ParenthesesSelectTableExpression} '(' value=SelectStatementRootExpression ')'
;

UnionSetOperator infers SetOperator:
    operator=('UNION' | 'EXCEPT' | 'MINUS') ('ALL' | distinct?='DISTINCT')?
;

IntersectSetOperator infers SetOperator:
    operator='INTERSECT' ('ALL' | distinct?='DISTINCT')?
;

SimpleSelectStatement:
    'SELECT'
    top=TopClause?
    select=SelectElements
    from=FromClause?
    where=WhereClause?
    groupBy=GroupByClause?
    having=HavingClause?
    orderBy=OrderByClause?
    (
        limit=LimitClause
        | fetchFirst=FetchFirstClause
    )?
;

/**
 * SQL Server `FETCH FIRST` syntax
 */
TopClause:
    'TOP' value=NUMBER percent?='PERCENT'?;

/**
 * MySQL/PostreSQL `FETCH FIRST` syntax
 */
LimitClause:
    'LIMIT' (offset=NUMBER ',' limit=NUMBER | limit=NUMBER ('OFFSET' offset=NUMBER)?);

/**
 * Oracle/Standard `FETCH FIRST` syntax
 */
FetchFirstClause:
    ('OFFSET' offset=NUMBER offsetRows?='ROWS'?)? 'FETCH' (first?='FIRST' | 'NEXT') limit=NUMBER 'ROWS' (only?='ONLY' | 'WITH' 'TIES');

WithClause:
    'WITH' 'RECURSIVE'? ctes+=CommonTableExpression (',' ctes+=CommonTableExpression)*;

CteColumnName:
    name=Identifier;

CommonTableExpression:
    name=Identifier
    ('(' columnNames+=CteColumnName (',' columnNames+=CteColumnName)* ')')?
    'AS' statement=SelectStatementRootExpression
;

WhereClause:
    'WHERE' rowCondition=Expression;

HavingClause:
    'HAVING' groupCondition=Expression
;

GroupByClause:
    'GROUP' 'BY' specs+=Expression (',' specs+=Expression)*
;

OrderByClause:
    'ORDER' 'BY' specs+=OrderBySpec (',' specs+=OrderBySpec)*
;

OrderBySpec:
    scalar=Expression ('ASC'|'DESC')?
;

SelectElements:
    ('ALL'|distinct?='DISTINCT')? elements+=SelectElement (',' elements+=SelectElement)*
;

SelectElement infers SelectElement:
    {infer AllStar} '*'
    | {infer AllTable} variableName=[TableVariableSource:Identifier] '.' '*'
    | {infer ExpressionQuery} expr=Expression ('AS'? name=Identifier)?
;

FromClause:
    'FROM' sources=TableSources;

TableSources:
    list+=TableSource (',' list+=TableSource)*;

TableSource:
    item=SourceItem joins+=JoinPart*
;

JoinPart:
    (leftJoin?='LEFT' | rightJoin?='RIGHT')? 'JOIN' nextItem=SourceItem ('ON' joinOn=Expression | 'USING' '(' usingOn=Expression ')')?
;

SourceItem infers SourceItem:
    {infer TableSourceItem} table=GlobalReference ('AS'? name=Identifier)?
    | {infer SubQuerySourceItem} '(' subQuery=SelectStatementRootExpression ')' ('AS'? name=Identifier)?
;

type TableVariableSource = TableSourceItem | SubQuerySourceItem;

type TableLike = TableDefinition | CommonTableExpression;

Expression:
    ConcatExpression;

ConcatExpression infers Expression:
    OrExpression ({infer BinaryExpression.left=current} operator='||' right=OrExpression)*
;

OrExpression infers Expression:
    AndExpression ({infer BinaryExpression.left=current} operator='OR' right=AndExpression)*
;

AndExpression infers Expression:
    EqExpression ({infer BinaryExpression.left=current} operator='AND' right=EqExpression)*
;

EqExpression infers Expression:
    RelationalExpression ({infer BinaryExpression.left=current} operator=('='|'<>') right=RelationalExpression)*
;

RelationalExpression infers Expression:
    NegatableExpression ({infer BinaryExpression.left=current} operator=('<='|'>='|'<'|'>') right=NegatableExpression)*
;

NegatableExpression infers Expression:
    AdditiveExpression ({infer NegatableExpression.left=current} (operator=('IS') negated?='NOT'? | negated?='NOT'? operator=('IN'|'LIKE')) right=AdditiveExpression)*
    | AdditiveExpression ({infer BetweenExpression.subExpr=current} negated?='NOT'? 'BETWEEN' left=AdditiveExpression 'AND' right=AdditiveExpression)
;

AdditiveExpression infers Expression:
    MultiplicativeExpression ({infer BinaryExpression.left=current} operator=('+'|'-') right=MultiplicativeExpression)*
;

MultiplicativeExpression infers Expression:
    JsonExtractExpression ({infer BinaryExpression.left=current} operator=('*'|'/'|'%') right=JsonExtractExpression)*
;

//Syntax for SingleStore SQL (https://docs.singlestore.com/managed-service/en/reference/sql-reference/json-functions/json_extract_-type-.html)
JsonExtractExpression infers Expression:
    PrimaryExpression ({infer BinaryExpression.left=current} operator=('::$'|'::%'|'::') right=(StringLiteral|NumberLiteral|IdentifierAsStringLiteral))*
;

IdentifierAsStringLiteral infers Expression:
    {infer IdentifierAsStringLiteral} value=Identifier
;

PrimaryExpression infers Expression:
    {infer StringLiteral} StringLiteral
    | {infer TableRelatedColumnExpression} variableName=[SourceItem:Identifier] '.' columnName=[ColumnNameSource:Identifier]
    | {infer NumberLiteral} NumberLiteral
    | {infer BooleanLiteral} BooleanLiteral
    | {infer CastExpression} CastExpression
    | {infer UnaryExpression} operator=('NOT'|'-'|'+') value=PrimaryExpression
    | {infer ParenthesisOrListExpression} '(' items+=Expression (',' items+=Expression)* ')'
    | {infer SubQueryExpression} '(' subQuery=SelectStatementRootExpression ')'
    | {infer FunctionCall} function=GlobalReference '(' params=SelectElements? ')' 
      overClause=OverClause?
    | {infer ColumnNameExpression} columnName=[ColumnNameSource:Identifier]
    | {infer NullLiteral} 'NULL'
    | {infer HexStringLiteral} HexStringLiteral
;

OverClause:
    'OVER' ('(' spec=WindowSpec ')' | windowName=[WindowSpec:Identifier])
;


WindowSpec:
    name=Identifier? PartitionClause? OrderByClause? FrameClause?
;

FrameClause:
    units=FrameUnits extent=FrameExtent
;

FrameUnits returns string:
    'ROWS' | 'RANGE'
;

FrameExtent:
    FrameRange | FrameBetween
;

FrameBetween:
    'BETWEEN' FrameRange 'AND' FrameRange
;

FrameRange:
    'CURRENT' 'ROW'
    | 'UNBOUNDED' ('PRECEDING' | 'FOLLOWING')
    | Expression ('PRECEDING' | 'FOLLOWING')
;

PartitionClause:
    'PARTITION' 'BY' exprs+=Expression (',' exprs+=Expression)*
;

CastExpression:
    'CAST' '(' expr=Expression 'AS' type=Type ')'
;

type ColumnNameSource = ColumnDefinition | ExpressionQuery;

Identifier returns string:
    ID | QUOTED_ID | TICK_STRING | BRACKET_STRING
;

NumberLiteral:
    value=NUMBER;
StringLiteral:
    value=STRING;
BooleanLiteral:
    value?='TRUE' | 'FALSE';
HexStringLiteral:
    value=HEX_STRING;

Type: DataType;

DataType: dataTypeNames+=ID+ ('(' arguments+=DataTypeArgument (',' arguments+=DataTypeArgument)? ')')?;

DataTypeArgument:
    NumberLiteral | StringLiteral;

hidden terminal WS: /\s+/;
terminal HEX_STRING returns string: /x\'[A-Fa-f0-9]+\'/;
terminal STRING returns string: /'(\\.|[^'\\])*'/;
terminal TICK_STRING returns string: /\`(\\.|\\\\|[^`\\])*\`/;
terminal BRACKET_STRING returns string:    /\[(\\.|\\\\|[^[\\])*\]/;
terminal QUOTED_ID returns string: /\"(\\.|\\\\|[^"\\])*\"/;
terminal ID: /[_a-zA-Z][\w_]*/;
terminal NUMBER returns number: /\d+((\.\d+)?([eE][\-+]?\d+)?)?/;

hidden terminal ML_COMMENT: /\/\*[\s\S]*?\*\//;
hidden terminal TICK_COMMENT: /\-\-[^\n\r]*/;
hidden terminal SL_COMMENT: /\/\/[^\n\r]*/;
