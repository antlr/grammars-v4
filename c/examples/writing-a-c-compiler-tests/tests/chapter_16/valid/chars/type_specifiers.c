// make sure we can parse different ways to specify signed & unsigned char

#ifdef SUPPRESS_WARNINGS
#ifndef __clang__
#pragma GCC diagnostic ignored "-Wold-style-declaration"
#endif
#endif

char signed static a = 10;
unsigned static char b = 20;
char c = 30;

int main(void)
{
    extern signed char a;
    char unsigned extern b;
    extern char c;
    if (a != 10) {
        return 1;
    }

    if (b != 20) {
        return 2;
    }

    if (c != 30) {
        return 3;
    }

    int loop_counter = 0;

    // declare a character in a for loop header
    for (unsigned char d = 0; d < 100; d = d + 1) {
        loop_counter = loop_counter + 1;
    }

    if (loop_counter != 100) {
        return 4;
    }

    return 0;
}