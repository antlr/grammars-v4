// Test that we correctly get the size of ++ and -- expressions (and don't evaluate them)

#if defined SUPPRESS_WARNINGS && defined __clang__
#pragma clang diagnostic ignored "-Wunevaluated-expression"
#endif

int main(void) {
    int i = 0;
    long l = 0;
    static char arr[3] = {0, 0, 0};
    char *ptr = arr;
    if (sizeof (i++) != 4) {
        return 1; // fail
    }

    if (sizeof (arr[0]--) != 1) {
        return 2; // fail
    }


    if (sizeof (++l) != 8) {
        return 3; // fail
    }

    if (sizeof (--arr[1]) != 1) {
        return 4; // fail
    }

    if (sizeof (ptr--) != 8) {
        return 5;
    }

    // make sure we didn't actually increment/decrement anything

    if (i) {
        return 6; // fail
    }

    if (l) {
        return 7; // fail
    }

    if (arr[0] || arr[1] || arr[2]) {
        return 8; // fail
    }

    if (ptr != arr) {
        return 9; // fail
    }

    return 0; // success
}