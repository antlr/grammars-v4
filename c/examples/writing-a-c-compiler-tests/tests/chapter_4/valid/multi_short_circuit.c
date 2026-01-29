#ifdef SUPPRESS_WARNINGS
#ifdef __clang__
#pragma clang diagnostic ignored "-Wlogical-op-parentheses"
#else
#pragma GCC diagnostic ignored "-Wparentheses"
#endif
#endif
int main(void) {
    return 0 || 0 && (1 / 0);
}