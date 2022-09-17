D                       [0-9]
L                       [a-zA-Z_]
E                       [DdEe][+-]?{D}+

%{
/** 
 By Danny Luk 12/1995
 (1) ignore comments and system calls 
**/

#include "y.tab.h"

void count();
%}

%START ST TR
%%
"%".*$                  { ECHO; }
"!".*$                  { ECHO; }
"...".*\n               { count(); /* return(CONTINUATION); */ }

"break"                 { count(); BEGIN ST; return(BREAK); }
"clear"                 { count(); BEGIN ST; return(CLEAR); }
"else"                  { count(); BEGIN ST; return(ELSE); }
"end"                   { count(); BEGIN ST; return(END); }
"elseif"                { count(); BEGIN ST; return(ELSEIF); }
"for"                   { count(); BEGIN ST; return(FOR); }
"function"              { count(); BEGIN ST; return(FUNCTION); }
"global"                { count(); BEGIN ST; return(GLOBAL); }
"if"                    { count(); BEGIN ST; return(IF); }
"return"                { count(); BEGIN ST; return(RETURN); }
"while"                 { count(); BEGIN ST; return(WHILE); }

{L}({L}|{D})*           { count(); BEGIN TR; return(check_type()); }

{D}+({E})?              { count(); BEGIN TR; return(CONSTANT); }
{D}*"."{D}+({E})?       { count(); BEGIN TR; return(CONSTANT); }
{D}+"."{D}*({E})?       { count(); BEGIN TR; return(CONSTANT); }

<ST>'[^'\n]*'/'         { BEGIN ST; yymore(); }
<ST>'[^'\n]*'           { count(); BEGIN 0;  return(STRING_LITERAL); }
<TR>'                   { count(); BEGIN TR; return(TRANSPOSE); }

".*"                    { count(); BEGIN ST; return(ARRAYMUL); }
".^"                    { count(); BEGIN ST; return(ARRAYPOW); }
"./"                    { count(); BEGIN ST; return(ARRAYDIV); }
".\\"                   { count(); BEGIN ST; return(ARRAYRDIV); }
".'"                    { count(); BEGIN TR; return(TRANSPOSE); }
"<="                    { count(); BEGIN ST; return(LE_OP); }
">="                    { count(); BEGIN ST; return(GE_OP); }
"=="                    { count(); BEGIN ST; return(EQ_OP); }
"~="                    { count(); BEGIN ST; return(NE_OP); }
"~"                     { count(); BEGIN ST; return('~'); }
";"                     { count(); BEGIN ST; return(';'); }
","                     { count(); BEGIN ST; return(','); }
":"                     { count(); BEGIN ST; return(':'); }
"="                     { count(); BEGIN ST; return('='); }
"("                     { count(); BEGIN ST; return('('); }
")"                     { count(); BEGIN TR; return(')'); }
"["                     { count(); BEGIN ST; return('['); }
"]"                     { count(); BEGIN TR; return(']'); }
"&"                     { count(); BEGIN ST; return('&'); }
"-"                     { count(); BEGIN ST; return('-'); }
"+"                     { count(); BEGIN ST; return('+'); }
"*"                     { count(); BEGIN ST; return('*'); }
"/"                     { count(); BEGIN ST; return('/'); }
"\\"                    { count(); BEGIN ST; return('\\'); }
"<"                     { count(); BEGIN ST; return('<'); }
">"                     { count(); BEGIN ST; return('>'); }
"^"                     { count(); BEGIN ST; return('^'); }
"|"                     { count(); BEGIN ST; return('|'); }
\n                      { count(); BEGIN ST; return(CR); }
[ \t\v\f]               { count(); BEGIN ST; }
.                       { BEGIN ST; /* ignore bad characters */ }

%%

yywrap()
{
        return(1);
}

int column = 0;

void count()
{
        int i;

        for (i = 0; yytext[i] != '\0'; i++)
                if (yytext[i] == '\n')
                        column = 0;
                else if (yytext[i] == '\t')
                        column += 8 - (column % 8);
                else
                        column++;

        ECHO;
}

/*--------------------------------------------------------------*/
int check_type()
{
/*
* pseudo code --- this is what it should check
*
*       if (yytext == type_name)
*               return(TYPE_NAME);
*
*       return(IDENTIFIER);
*/

/*
*       it actually will only return IDENTIFIER
*/

        return(IDENTIFIER);
}



