#ifdef SUPPRESS_WARNINGS
#ifndef __clang__
#pragma GCC diagnostic ignored "-Wparentheses"
#endif
#endif
int main(void) {
    return (0 == 0 && 3 == 2 + 1 > 1) + 1;
}