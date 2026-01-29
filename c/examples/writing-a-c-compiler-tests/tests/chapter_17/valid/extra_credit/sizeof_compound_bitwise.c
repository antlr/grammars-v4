// Test that we correctly get the size of compound bitwise operations
// (and don't evaluate them)

#if defined SUPPRESS_WARNINGS && defined __clang__
#pragma clang diagnostic ignored "-Wunevaluated-expression"
#endif

int main(void) {
    static signed char sc = 10;
    unsigned int u = 10000u;
    long l = -99999;

    if (sizeof(sc &= l) != 1) {
        return 1;  // fail
    }

    if (sizeof(l |= u) != 8) {
        return 2;  // fail
    }

    if (sizeof(u ^= l) != 4) {
        return 3;  // fail
    }
    if (sizeof(l >>= sc) != 8) {
        return 4;
    }
    if (sizeof(sc <<= sc) != 1) {
        return 5;
    }

    // make sure we didn't perform updates
    if (sc != 10) {
        return 6;  // fail
    }
    if (u != 10000u) {
        return 7;  // fail
    }
    if (l != -99999) {
        return 8;  // fail
    }

    return 0;
}