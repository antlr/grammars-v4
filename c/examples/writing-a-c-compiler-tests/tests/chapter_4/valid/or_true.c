#ifdef SUPPRESS_WARNINGS
#ifdef __clang__
#pragma clang diagnostic ignored "-Wconstant-logical-operand"
#endif
#endif

int main(void) {
    return (4 || 0) + (0 || 3) + (5 || 5);
}