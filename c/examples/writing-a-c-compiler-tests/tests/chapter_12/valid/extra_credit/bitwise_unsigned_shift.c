/* Tests for bit-shift operations on unsigned integers */

int main(void) {
    unsigned int ui = -1u;  // 2^32 - 1, or 4294967295

    /* Shifting left by 2 is like subtracting 3;
     * note that we don't cast ui to a long first.
     * Also note that the result wraps around; it's
     * equivalent to (ui * 2^2) % UINT_MAX.
     */
    if ((ui << 2l) != 4294967292) {
        return 1;
    }

    /* Shifting right by 2 is like dividing by 4;
     * note that we need to use shr (logical shift) instruction
     * rather than sar (arithmetic shift) instruction  */
    if ((ui >> 2) != 1073741823) {
        return 2;
    }

    /* Test unsigned shift with variable shift counts, to make sure we handle
     * them correctly in codegen/code emission */
    static int shiftcount = 5;
    if ((1000000u >> shiftcount) != 31250) {
        return 3;
    }

    if ((1000000u << shiftcount) != 32000000) {
        return 4;
    }

    return 0;  // success
}