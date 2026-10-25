grammar SimpleCalc;
 
tokens {
    PLUS    = '+' ;
    MINUS   = '-' ;
    MULT    = '*' ;
    DIV = '/' ;
}
 
@members {
    public static void main(String[] args) throws Exception {
        SimpleCalcLexer lex = new SimpleCalcLexer(new ANTLRFileStream(args[0]));
        CommonTokenStream tokens = new CommonTokenStream(lex);
 
        SimpleCalcParser parser = new SimpleCalcParser(tokens);
 
        try {
            parser.expr();
        } catch (RecognitionException e)  {
            e.printStackTrace();
        }
    }
}
 
/*------------------------------------------------------------------
 * PARSER RULES
 *------------------------------------------------------------------*/
 
expr    : term ( ( PLUS | MINUS )  term )* ;
 
term    : factor ( ( MULT | DIV ) factor )* ;
 
factor  : NUMBER ;
 
 
/*------------------------------------------------------------------
 * LEXER RULES
 *------------------------------------------------------------------*/
 
NUMBER  : (DIGIT)+ ;
 
WHITESPACE : ( '\t' | ' ' | '\r' | '\n'| '\u000C' )+    { $channel = HIDDEN; } ;
 
fragment DIGIT  : '0'..'9' ;

