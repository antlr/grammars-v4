grammar Simple;

root:
    expression1 EOF
;

expression:
    ('+'|'-') expression
    |atom
    |expression ('*'|'/') expression
    |expression ('+'|'-') expression
    |'(' expression ')'
;

atom:
    ID #simpleatom
;

expression1:
    atom1
    |('+'|'-') expression1
    |'(' expression1 ')'
    |linkedexp
;

linkedexp:
        expression1 //(('*'|'-'|'+'|'/') abt)+
        |('*'|'-'|'+'|'/') expression1
;

brackets:     '(' expression1 ')'
;
atom1:
    ID
    |ID '(' expression1 ')'
//    | ID '(' expression1 ')'#funccall
;

ID:                  ( [A-Za-z_#] ) ( [A-Za-z_#$@0-9] )*;
