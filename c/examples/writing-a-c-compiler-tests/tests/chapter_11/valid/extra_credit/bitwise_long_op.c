/* Test bitwise &, |, and ^ operations on long integers.
 * Make sure we:
 * - promote both operands to a common type;
 * - actually perform quadword (not longword) operations
 * - use appropriate rewrite rules where one operand is an
 *   immediate that can't fit in a signed 32-bit integer
 */
int main(void) {
    // basic tests to make sure we're performing quadword operations
    long l1 = 71777214294589695l;  // 0x00ff_00ff_00ff_00ff
    long l2 = -4294967296;  // -2^32; upper 32 bits are 1, lower 32 bits are 0

    if ((l1 & l2) != 71777214277877760l /* 0x00ff_00ff_0000_0000 */) {
        return 1;
    }

    if ((l1 | l2) != -4278255361 /* 0xffff_ffff_00ff_00ff */) {
        return 2;
    }

    if ((l1 ^ l2) != -71777218556133121 /* 0xff00_ff00_00ff_00ff */) {
        return 3;
    }

    /* Rewrite rules: andq $IMM, m64 doesn't work if $IMM can't fit
     * in signed 32-bit int. Ditto for orq and xorq */
    if ((-1l & 34359738368l) != 34359738368l) {  // 34359738368 == 2^35
        return 4;
    }

    if ((0l | 34359738368l) != 34359738368l) {
        return 5;
    }

    // 137438953472 == 2^37;
    if ((34359738368l ^ 137438953472l) != 171798691840l) {
        return 6;
    }

    /* Typechecking: promote both operands to common type */
    long l = 4611686018427387903l;  // 0x3fff_ffff_ffff_ffff
    // if we try to use i in longword bitwise op without sign-extending it
    // first, we may try to read neighboring values l and i2
    int i = -1073741824;  // 0b1100....0, or 0xc000_0000
    int i2 = -1;

    // 1. sign-extend i to 64 bits; upper 32 bits are all 1s
    // 2. take bitwise AND of sign-extended value with l
    // 3. result is 0x3fff_ffff_c000_0000; upper bits match l, lower bits match i
    if ((i & l) != 4611686017353646080l) {
        return 7;
    }

    // i is sign-extended so upper bytes are 1s; lower bytes of l are 1s
    if ((l | i) != -1) {
        return 8;
    }

    // 0x3fff_ffff_ffff_ffff ^ 0xffff_ffff_c000_0000 = 0xc000_0000_3fff_ffff
    if ((l ^ i) != -4611686017353646081) {
        return 9;
    }

    // 1. sign extend i2; value is still -1
    // 2. XOR result w/ 0x3fff_ffff_ffff_ffff (as a constant this time)
    // 3. result is the same as taking bitwise complement of 0x3fff_ffff_ffff_ffff
    if ((i2 ^ 4611686018427387903l) != ~4611686018427387903l) {
        return 10;
    }

    return 0;  // success
}