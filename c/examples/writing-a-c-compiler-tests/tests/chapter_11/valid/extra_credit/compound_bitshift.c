// Test compound bit shift operators with mix of types

int main(void) {

    // shift int using long shift count
    int x = 100;
    x <<= 22l;
    if (x != 419430400) {
        return 1; // fail
    }

    // try right shift; validate result of expression
    if ((x >>= 4l) != 26214400) {
        return 2; // fail
    }

    // also validate side effect of updating variable
    if (x != 26214400) {
        return 3;
    }

    // now try shifting a long with an int shift count
    long l = 12345l;
    if ((l <<= 33) != 106042742538240l) {
        return 4;
    }

    l = -l;
    if ((l >>= 10) != -103557365760l) {
        return 5;
    }

    return 0; // success
}