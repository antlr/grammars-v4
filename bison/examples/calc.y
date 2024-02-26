/* Infix notation calculator--calc */

%{
#define YYSTYPE double
#include <math.h>
%}

/* BISON Declarations */
%token NUM
   %left '-' '+'
   %left '*' '/'
   %left NEG     /* negation--unary minus */
   %right '^'    /* exponentiation        */

/* Grammar follows */
%%
input
    : /* empty string */
    | input line
    ;

line
    : '\n'
    | exp '\n'  { printf ("\t%.10g\n", $1); }
    ;

exp
    : NUM                { $$ = $1;         }
    | exp '+' exp        { $$ = $1 + $3;    }
    | exp '-' exp        { $$ = $1 - $3;    }
    | exp '*' exp        { $$ = $1 * $3;    }
    | exp '/' exp        { $$ = $1 / $3;    }
    | '-' exp  %prec NEG { $$ = -$2;        }
    | exp '^' exp        { $$ = pow ($1, $3); }
    | '(' exp ')'        { $$ = $2;         }
    ;

%%