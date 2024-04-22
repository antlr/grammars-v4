grammar YamlGrammar;

yaml
   : document+
   ;

document
   : key_value_pairs EOF
   ;

key_value_pairs
   : key_value_pair
   | key_value_pairs key_value_pair
   ;

key_value_pair
   : key ':' value
   ;

key
   : STRING
   ;

value
   : STRING
   | NUMBER
   | BOOLEAN
   | list
   | key_value_pairs
   ;

list
   : '[' value (',' value)* ']'
   ;

STRING
   : '"' ~ ["]* '"'
   ;

NUMBER
   : ('-'? [0-9]+ '.' [0-9]+)
   ;

BOOLEAN
   : ('true' | 'false')
   ;

WS
   : [ \t\r\n]+ -> skip
   ;

COMMENT
   : '#' ~ [\r\n]* -> skip
   ;

