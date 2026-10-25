#ifdef SUPPRESS_WARNINGS
#ifdef __clang__
#pragma clang diagnostic ignored "-Wconstant-logical-operand"
#endif
#pragma GCC diagnostic ignored "-Wbool-operation"
#endif
int main(void) {
    return ~(0 && 1) - -(4 || 3);
}