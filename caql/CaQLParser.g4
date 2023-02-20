parser grammar CaQLParser;

options { tokenVocab = CaQLLexer; }

expression: vectorOperation;

// Binary operations are ordered by precedence

// Unary operations have the same precedence as multiplications

vectorOperation
    : <assoc=right> vectorOperation (powOp vectorOperation | subqueryOp) # assocRight
    | unaryOp vectorOperation   # unary
    | vectorOperation multOp vectorOperation #mult
    | vectorOperation addOp vectorOperation  #add
    | vectorOperation compareOp vectorOperation # compare
    | vectorOperation andUnlessOp vectorOperation # andUnless
    | vectorOperation orOp vectorOperation #or
    | vectorOperation vectorMatchOp vectorOperation #match

    | FUNCTION LEFT_BRACE vectorOperation multMetrics*  RIGHT_BRACE # opMethod
    | LEFT_PAREN vectorOperation RIGHT_PAREN #paren
    | FUNCTION LEFT_PAREN  metricsName=STRING COMMA  condition=STRING (COMMA LIMIT size=NUMBER)? RIGHT_PAREN metricsAggregation* #fuction
    | vectorOperation metricsAggregation # pip
    | NUMBER #number
    ;
// Operators


unaryOp:        ADD | SUB;
powOp:          POW grouping?;
multOp:         (MULT | DIV | MOD) grouping?;
addOp:          (ADD | SUB) grouping?;
compareOp:      (DEQ | NE | GT | LT | GE | LE) BOOL? grouping?;
andUnlessOp:    (AND | UNLESS) grouping?;
orOp:           OR grouping?;
vectorMatchOp:  (ON | UNLESS) grouping?;
subqueryOp:     SUBQUERY_RANGE offsetOp?;
offsetOp:       OFFSET DURATION;

// Functions

parameter:     literal | vectorOperation;
parameterList: LEFT_PAREN (parameter (COMMA parameter)*)? RIGHT_PAREN;
metricsAggregation: LINE_ AGGREGATION_OPERATOR parameterList;
multMetrics: COMMA vectorOperation;


by:      BY labelNameList;
without: WITHOUT labelNameList;

// Vector one-to-one/one-to-many joins

grouping:   (on_ | ignoring) (groupLeft | groupRight)?;
on_:         ON labelNameList;
ignoring:   IGNORING labelNameList;
groupLeft:  GROUP_LEFT labelNameList?;
groupRight: GROUP_RIGHT labelNameList?;

// Label names

labelName:     keyword | METRIC_NAME | LABEL_NAME;
labelNameList: LEFT_PAREN (labelName (COMMA labelName)*)? RIGHT_PAREN;

keyword
    : AND
    | OR
    | UNLESS
    | BY
    | WITHOUT
    | ON
    | IGNORING
    | GROUP_LEFT
    | GROUP_RIGHT
    | OFFSET
    | BOOL
    | AGGREGATION_OPERATOR
    | FUNCTION
    ;

literal: NUMBER | STRING;
