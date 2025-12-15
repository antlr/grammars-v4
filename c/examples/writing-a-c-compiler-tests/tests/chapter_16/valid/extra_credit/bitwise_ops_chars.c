// make sure we perform integer promotions when performing bitwise operations on
// chars

int main(void) {
    unsigned char uc = 135;
    char c = -116;
    // Make sure we FIRST promote, THEN perform bitwise op
    // if we performed the bitwise op first and then sign-extended,
    // we'd get a different result
    if ((uc & c) != 132) {
        return 1;  // fail
    }

    // Make sure we FIRST promote, THEN perform bitwise op
    // if we performed the bitwise op first and then zero-extended,
    // we'd get a different result
    if ((uc | c) != -113) {
        return 2;  // fail
    }

    // make sure we do usual conversion to common type
    // (result of c ^ 1001u is unsigned int, so we zero-extend it
    // for next operation)
    if (((c ^ 1001u) | 360l) != 4294966637) {
        return 3; // fail
    }

    return 0;
}