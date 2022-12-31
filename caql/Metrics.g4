grammar Metrics;

expr:
    op=(AND_|OR_) LEFT_PAREN_ metricsName=STRING_ COLON_ metricsValue=STRING_ (metricsMult)* RIGHT_PAREN_
    ;
metricsMult: COMMA_ metricsName=STRING_ COLON_ metricsValue=STRING_;
LEFT_PAREN_:  '(';
RIGHT_PAREN_: ')';
AND_:    'and';
OR_:     'or';
COMMA_: ',';
STRING_: [a-zA-Z0-9_*=/"]+[a-zA-Z0-9_*=/ "]+[a-zA-Z0-9_*=/"]+;
COLON_: ':';

WS_: [ \t\n\r\u000C]+ -> skip;
SHEBANG : '#' '!' ~('\n'|'\r')* -> channel(HIDDEN);
