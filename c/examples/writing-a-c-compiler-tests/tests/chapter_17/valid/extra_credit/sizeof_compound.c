// Test that we correctly get size of compound expressions (and don't evaluate
// them)

#if defined SUPPRESS_WARNINGS && defined __clang__
#pragma clang diagnostic ignored "-Wunevaluated-expression"
#endif

int main(void) {
    long long_arr[2] = {1, 2};
    static int i = 3;
    static unsigned char uc = 4;
    double d = 5.0;
    long *ptr = long_arr;

    if (sizeof(long_arr[1] *= 10) != 8) {
        return 1;  // fail
    }
    if (sizeof(i /= 10ul) != 4) {
        return 2;  // fail
    }
    if (sizeof(uc %= 2) != 1) {
        return 3;  // fail
    }
    if (sizeof(d -= 11) != 8) {
        return 4;  // fail
    }
    if (sizeof(ptr += 1) != 8) {
        return 5;  // fail
    }

    // make sure we didn't actually evaluate any sizeof operands
    if (long_arr[0] != 1) {
        return 6;  // fail
    }
    if (long_arr[1] != 2) {
        return 7;  // fail
    }
    if (i != 3) {
        return 8;  // fail
    }
    if (uc != 4) {
        return 9;  // fail
    }
    if (d != 5.0) {
        return 10;  // fail
    }
    if (ptr != long_arr) {
        return 11;  // fail
    }

    return 0;  // success
}