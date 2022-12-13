grammar Metrics;

expr:
    op=(AND|OR) LEFT_PAREN metricsName=STRING COLON metricsValue=STRING (metricsMult)* RIGHT_PAREN
    ;
metricsMult: COMMA metricsName=STRING COLON metricsValue=STRING;
LEFT_PAREN:  '(';
RIGHT_PAREN: ')';
AND:    'and';
OR:     'or';
COMMA: ',';
STRING: [a-zA-Z0-9_*=/"]+[a-zA-Z0-9_*=/ "]+[a-zA-Z0-9_*=/"]+;
COLON: ':';

WS: [ \t\n\r\u000C]+ -> skip;
SHEBANG : '#' '!' ~('\n'|'\r')* -> channel(HIDDEN);
