#ifdef SUPPRESS_WARNINGS
#ifdef __clang__
#pragma clang diagnostic ignored "-Wshift-op-parentheses"
#else
#pragma GCC diagnostic ignored "-Wparentheses"
#endif
#endif

int main(void) {
    return 40 << 4 + 12 >> 1;
}