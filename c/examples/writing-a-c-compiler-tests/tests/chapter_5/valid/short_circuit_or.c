#ifdef SUPPRESS_WARNINGS
#ifndef __clang__
#pragma GCC diagnostic ignored "-Wunused-value"
#endif
#endif
int main(void) {
    int a = 0;
    1 || (a = 1);
    return a;
}