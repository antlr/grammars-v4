#ifdef SUPPRESS_WARNINGS
#ifdef __clang__
#pragma clang diagnostic ignored "-Wconstant-logical-operand"
#endif
#endif

int main(void) {
    int a;
    a = 0 || 5;
    return a;
}