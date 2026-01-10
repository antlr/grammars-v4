#ifdef SUPPRESS_WARNINGS
#ifdef __clang__
#pragma clang diagnostic ignored "-Wparentheses"
#else
#pragma GCC diagnostic ignored "-Wparentheses"
#endif
#endif

int main(void) {
    return 5 >= 0 > 1 <= 0;
}