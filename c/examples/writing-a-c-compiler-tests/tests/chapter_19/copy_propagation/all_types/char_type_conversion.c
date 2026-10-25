/* Test that we can propagate copies between char and signed char */
#ifdef SUPPRESS_WARNINGS
#ifdef __clang__
#pragma clang diagnostic ignored "-Wconstant-conversion"
#else
#pragma GCC diagnostic ignored "-Woverflow"
#endif
#endif

int putchar(int c);  // from standard library

void print_some_chars(char a, char b, char c, char d) {
    putchar(a);
    putchar(b);
    putchar(c);
    putchar(d);
}

int callee(char c, signed char s) {
    return c == s;
}

int target(char c, signed char s) {
    // first, call another function, with these arguments
    // in different positions than in target or callee, so we can't
    // coalesce them with the param-passing registers or each other
    print_some_chars(67, 66, c, s);

    s = c;  // generate s = c - we can do this because for the purposes of copy
            // propagation, we consider char and signed char the same type

    // both arguments to callee should be the same
    return callee(s, c);
}

int main(void) {
    return target(65, 64);
}