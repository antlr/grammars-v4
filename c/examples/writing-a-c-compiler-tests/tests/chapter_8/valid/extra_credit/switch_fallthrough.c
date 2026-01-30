#ifdef SUPPRESS_WARNINGS
#ifndef __clang__
#pragma GCC diagnostic ignored "-Wimplicit-fallthrough"
#endif
#endif

int main(void) {
    int a = 4;
    int b = 9;
    int c = 0;
    switch (a ? b : 7) {
        case 0:
            return 5;
        case 7:
            c = 1;
        case 9:
            c = 2;
        case 1:
            c = c + 4;
    }
    return c;
}