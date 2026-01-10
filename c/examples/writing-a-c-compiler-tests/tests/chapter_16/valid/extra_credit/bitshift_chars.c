// Test << and >> operators with chars (or mix of chars and other types)
// Make sure left operand is promoted from char to int (NOTE: right operand should
// be too but we can't verify that; it doesn't change the operand's value or
// the result value.)

int main(void) {
    unsigned char uc = 255;

    // uc is promoted to int
    //if ((uc << 20) != 267386880) {
    //    return 1; // fail
   // }

    // uc is promoted to int, then shifted
    if ((uc >> 3) != 31) {
        return 2; // fail
    }

    signed char sc = -127;
    char c = 5;
    // sc is promoted to int, then shifted
    if ((sc >> c) != -4) {
        return 3;  // fail
    }

    // make sure c << 3ul is promoted to int, not unsigned long
    if (((-(c << 3ul)) >> 3) != -5) {
        return 4;  // fail
    }

    // make sure uc << 5u is promoted to int, not unsigned int
    if ((-(uc << 5u) >> 5u) != -255l) {
        return 5; // fail
    }

    return 0; // fail

}