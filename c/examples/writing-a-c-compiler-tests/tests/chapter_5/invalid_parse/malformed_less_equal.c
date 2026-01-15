int main(void)
{
    // make sure we lex < and = as two separate tokens
    // this really tests the lexing logic from previous chapter,
    // but the lexer didn't recognize = yet
    return 1 < = 2;
}