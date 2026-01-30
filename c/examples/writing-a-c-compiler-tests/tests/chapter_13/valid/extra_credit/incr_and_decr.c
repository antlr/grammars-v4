// test ++/-- with doubles
int main(void) {
    static double d = 0.75;
    // basic tests
    if (d++ != 0.75) {
        return 1;
    }
    if (d != 1.75) {
        return 2;
    }

    d = -100.2;
    if (++d != -99.2) {
        return 3;
    }
    if (d != -99.2) {
        return 4;
    }

    if (d-- != -99.2) {
        return 5;
    }
    if (d != -100.2) {
        return 6;
    }

    if (--d != -101.2) {
        return 7;
    }
    if (d != -101.2) {
        return 8;
    }

    // if initial value is very small, it may disappear due to rounding error
    // after incr/decr
    d = 0.000000000000000000001;
    d++;
    if (d != 1.0) {
        return 9;
    }

    // ++ and -- have no effect if gap between representable values is greater
    // than 1
    d = 10e20;
    d--;
    if (d != 10e20) {
        return 10;
    }
    return 0;
}