grammar Overrides;

structOrUnion
     : 'struct'
     ;

jumpStatement
     : (
         'continue'
         | 'break'
         | 'return' expression?
     ) ';'
     ;

statement
    : labeledStatement
    | compoundStatement
    | expressionStatement
    | selectionStatement
    | iterationStatement
    | jumpStatement
    | ('__asm' | '__asm__') ('volatile' | '__volatile__') '(' (
        logicalOrExpression (',' logicalOrExpression)*
    )? (':' (logicalOrExpression (',' logicalOrExpression)*)?)* ')' ';'
    | trapStatement
    ;

trapStatement
    : 'trap' compoundStatement
    ;
