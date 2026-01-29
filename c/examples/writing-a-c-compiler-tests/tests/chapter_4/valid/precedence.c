#ifdef SUPPRESS_WARNINGS
#ifdef __clang__
#pragma clang diagnostic ignored "-Wconstant-logical-operand"
#pragma clang diagnostic ignored "-Wlogical-op-parentheses"
#else
#pragma GCC diagnostic ignored "-Wparentheses"
#endif
#endif

int main(void) {
    return 1 || 2 && 0;
}
