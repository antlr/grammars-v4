// Array arithmetic with prefix and postfix ++/--
int main(void) {
    double x[3] = {0.0, 1.0, 2.0};
    double *ptr = x;
    // prefix ++
    if (++ptr != x + 1) {
        return 1;  // fail
    }
    if (*ptr != 1.0) {
        return 2;  // fail
    }

    // postfix ++
    if (ptr++ != x + 1) {
        return 3;  // fail
    }
    if (ptr != x + 2) {
        return 4;
    }
    if (*ptr != 2.0) {
        return 5;  // fail
    }

    // prefix --
    if (--ptr != x + 1) {
        return 6;  // fail
    }
    if (*ptr != 1.0) {
        return 7;  // fail
    }

    // postfix--
    if (ptr-- != x + 1) {
        return 8;  // fail
    }
    if (*ptr != 0.0) {
        return 9;  // fail
    }
    if (ptr != x) {
        return 10;  // fail
    }

    return 0;  // success
}