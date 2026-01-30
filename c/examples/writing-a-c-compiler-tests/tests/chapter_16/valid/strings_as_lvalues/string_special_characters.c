/* Test that we can handle special characters in string literals
 * that are not array initializers
 */

#ifdef SUPPRESS_WARNINGS
#ifdef __clang__
#pragma clang diagnostic ignored "-Wincompatible-library-redeclaration"
#else
#pragma GCC diagnostic ignored "-Wbuiltin-declaration-mismatch"
#endif
#endif

int puts(char *s);
int strcmp(char *s1, char *s2);

int main(void) {
    // string literal containing escape sequences
    char *escape_sequence = "\a\b";
    if (escape_sequence[0] != 7) {
        return 1;
    }

    if (escape_sequence[1] != 8) {
        return 2;
    }

    if (escape_sequence[2]) {// check for terminating null byte
        return 3;
    }

    // double quote
    char *with_double_quote = "Hello\"world";
    if (with_double_quote[5] != '"') {
        return 4;
    }
    puts(with_double_quote);

    // backslash
    char *with_backslash = "Hello\\World";
    if (with_backslash[5] != '\\') {
        return 5;
    }
    puts(with_backslash);

    char *with_newline = "Line\nbreak!";
    if (with_newline[4] != 10) {
        return 6;
    }
    puts(with_newline);

    // literal tab
    char *tab = "	";
    if (strcmp(tab, "\t")) {
        return 7;
    }

    /* Make sure we can handle a special character immediately followed by a digit
    This is a regression test for a bug in the reference implementation:
    I used octal codes for all special characters but always pad them out to three digits, so
    e.g. " 123" was escaped as "\40123", which was interpreted as the invalid octal code \401
    followed by "23". The correct way to escape this is \040123, since the assembler recognizes
    a backslash followed by up to three octal digits as an escape sequence.
    */
   puts("Testing, 123.");
   /* Now test that we don't add a leading zero to an escape sequence that's already three digits.
   "^@" should be escaped as "\136\100". If we escape it as "\0136\0100" the assembler will interpret
   it as \013 6 \010 0
   */
    puts("^@1 _\\]");

    return 0;
}