#ifdef SUPPRESS_WARNINGS
#ifdef __clang__
#pragma clang diagnostic ignored "-Wswitch"
#else
#pragma GCC diagnostic ignored "-Woverflow"
#endif
#endif

// Make sure we promote the controlling condition in a switch statement from
// character type to int

int main(void) {
    char c = 100;
    switch (c) {
        case 0:
            return 1;
        case 100:
            return 0;
        // not a duplicate of 100, b/c we're not converting cases to char type
        case 356:
            return 2;
        default:
            return 3;
    }
}