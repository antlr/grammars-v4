#ifdef SUPPRESS_WARNINGS
#ifdef __clang__
#pragma clang diagnostic ignored "-Wconstant-conversion"
#else
#pragma GCC diagnostic ignored "-Woverflow"
#endif
#endif

// Test that we can use character constants in bitwise operations
int main(void) {
    int x = 10;
    if ((x ^ 'A') != 75) {
        return 1;  // fail
    }

    static char c = 132;  // converted to -124
    if (('!' | c) != -91) {
        return 2;  // fail
    }

    static unsigned long ul = 9259400834947493926ul;
    if ((ul & '~') != 38) {
        return 3;  // fail
    }

    if ((ul << ' ') != 4611738958194278400ul) {
        return 4;  // fail
    }

    if (('{' >> 3) != 15) {
        return 5;  // fail
    }

    return 0;
}