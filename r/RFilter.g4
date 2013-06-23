/** Works for all examples in
 http://www.mayin.org/ajayshah/KB/R/index.html
*/
parser grammar RFilter;

options { tokenVocab=R; }

@members {
protected int curlies = 0;
}

// TODO: MAKE THIS GET ONE COMMAND ONLY
stream : (elem|NL|';')* EOF ;

eat :   (NL {((WritableToken)$NL).setChannel(Token.HIDDEN_CHANNEL);})+ ;

elem:   op eat?
    |   atom
    |   '{' eat? {curlies++;} (elem|NL|';')* {curlies--;} '}'
    |   '(' (elem|eat)* ')'
    |   '[' (elem|eat)* ']'
    |   '[[' (elem|eat)* ']' ']'
    |   'function' eat? '(' (elem|eat)* ')' eat?
    |   'for' eat? '(' (elem|eat)* ')' eat?
    |   'while' eat? '(' (elem|eat)* ')' eat?
    |   'if' eat? '(' (elem|eat)* ')' eat?
    |   'else'
        {
        // ``inside a compound expression, a newline before else is discarded,
        // whereas at the outermost level, the newline terminates the if
        // construction and a subsequent else causes a syntax error.''
        /*
        Works here
            if (1==0) { print(1) } else { print(2) }

        and correctly gets error here:

            if (1==0) { print(1) }
            else { print(2) }

        this works too:

            if (1==0) {
              if (2==0) print(1)
              else print(2)
            }
        */
        WritableToken tok = (WritableToken)_input.LT(-2);
        if (curlies>0&&tok.getType()==NL) tok.setChannel(Token.HIDDEN_CHANNEL);
        }
    ;

atom:   'next' | 'break' | ID | STRING | HEX | INT | FLOAT | COMPLEX | 'NULL'
    |   'NA' | 'Inf' | 'NaN' | 'TRUE' | 'FALSE'
    ;

op  :   '+'|'-'|'*'|'/'|'^'|'<'|'<='|'>='|'>'|'=='|'!='|'&'|'&&'|USER_OP|
        'repeat'|'in'|'?'|'!'|'='|':'|'~'|'$'|'@'|'<-'|'->'|'='|'::'|':::'|
        ','|'...'
    ;
