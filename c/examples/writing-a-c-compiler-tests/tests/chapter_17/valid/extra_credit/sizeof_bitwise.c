// Test that we correctly get the size of bitwise and bitshift expression
int main(void) {
    static long l = 0;
    int i = 0;
    static char c = 0;

    // result type for &, |, ^ is common type
    if (sizeof (c & i) != 4) {
        return 1;  // fail
    }

    if (sizeof (i | l) != 8) {
        return 2;  // fail
    }

    // character operands are promoted
    if (sizeof (c ^ c) != 4) {
        return 3;  // fail
    }

    // result type for <<, >> is type of left operand
    if (sizeof (i << l) != 4) {
        return 4; // fail
    }

    // character operands are promoted
    if (sizeof (c << i) != 4) {
        return 5; // fail
    }

    if (sizeof (l >> c) != 8) {
        return 6; // fail
    }

    return 0;
}