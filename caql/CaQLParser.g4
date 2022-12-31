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
    | LEFT_PAREN_ vectorOperation RIGHT_PAREN_ #paren
    | FUNCTION LEFT_PAREN_  metricsName=STRING_ COMMA_  condition=STRING_ (COMMA_ LIMIT size=NUMBER)? RIGHT_PAREN_ metricsAggregation* #fuction
    | vectorOperation metricsAggregation # pip
    | NUMBER #number
    ;
// Operators


unaryOp:        ADD | SUB;
powOp:          POW grouping?;
multOp:         (MULT | DIV | MOD) grouping?;
addOp:          (ADD | SUB) grouping?;
compareOp:      (DEQ | NE | GT | LT | GE | LE) BOOL? grouping?;
andUnlessOp:    (AND_ | UNLESS) grouping?;
orOp:           OR_ grouping?;
vectorMatchOp:  (ON | UNLESS) grouping?;
subqueryOp:     SUBQUERY_RANGE offsetOp?;
offsetOp:       OFFSET DURATION;

// Functions

parameter:     literal | vectorOperation;
parameterList: LEFT_PAREN_ (parameter (COMMA_ parameter)*)? RIGHT_PAREN_;
metricsAggregation: LINE_ AGGREGATION_OPERATOR parameterList;
multMetrics: COMMA_ vectorOperation;


by:      BY_ labelNameList;
without: WITHOUT_ labelNameList;

// Vector one-to-one/one-to-many joins

grouping:   (on_ | ignoring) (groupLeft | groupRight)?;
on_:         ON labelNameList;
ignoring:   IGNORING_ labelNameList;
groupLeft:  GROUP_LEFT labelNameList?;
groupRight: GROUP_RIGHT labelNameList?;

// Label names

labelName:     keyword | METRIC_NAME | LABEL_NAME;
labelNameList: LEFT_PAREN_ (labelName (COMMA_ labelName)*)? RIGHT_PAREN_;

keyword
    : AND_
    | OR_
    | UNLESS
    | BY_
    | WITHOUT_
    | ON
    | IGNORING_
    | GROUP_LEFT
    | GROUP_RIGHT
    | OFFSET
    | BOOL
    | AGGREGATION_OPERATOR
    | FUNCTION
    ;

literal: NUMBER | STRING_;
