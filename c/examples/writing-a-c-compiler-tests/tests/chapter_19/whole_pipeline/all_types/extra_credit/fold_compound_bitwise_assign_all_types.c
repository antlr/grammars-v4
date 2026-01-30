/* Test copy prop/constant folding of compound bitwise assignment with non-integer
 * types and type conversions
 * TODO: use templates for duplicate code between here and earlier chapters
 * instead of copy-paste (ditto for other extra-credit constant-folding tests too!)
 */

#ifdef SUPPRESS_WARNINGS
#ifdef __clang__
#pragma clang diagnostic ignored "-Wshift-count-overflow"
#endif
#endif

 // Similar to Chapter 16's compound_bitwise_ops_chars.c but modified to be
 // constant-foldable
int target_chars(void) {
    signed char c1 = -128;
    signed char c2 = -120;
    signed char c3 = -2;
    signed char c4 = 1;
    signed char c5 = 120;

    unsigned char u1 = 0;
    unsigned char u2 = 170;
    unsigned char u3 = 250;
    unsigned char u4 = 255;

    // apply bitwise ops to signed chars
    c1 ^= 12345; // well-defined b/c of integer promotions
    c2 |= u4;
    c3 &= u2 - (unsigned char)185;
    c4 <<= 7u; // this wraps around to -128; well-defined b/c of integer promotions
    // it's undefined for shift count to be greater than width of left operand,
    // but this is well-defined b/c of integer promotions
    c5 >>= 31;

    // apply bitwise ops to unsigned chars
    long x = 32;
    // it's undefined for shift count to be greater than width of left operand,
    // but this is well-defined b/c of integer promotions
    u4 <<= 12;
    u3 >>= (x - 1);
    u2 |= -399; // doesn't overflow b/c of integer promotion
    x = -4296140120l; // a number that doesn't fit in int or unsigned int
    u1 ^= x;

    // validate
    if (c1 != -71) {
        return 1; // fail
    }

    if (c2 != -1) {
        return 2; // fail
    }

    if (c3 != -16) {
        return 3; // fail
    }

    if (c4 != -128) {
        return 4; // fail
    }

    if (c5) {
        return 5; // fail
    }

    if (u1 != 168) {
        return 6; // fail
    }

    if (u2 != 251) {
        return 7; // fail
    }

    if (u3) {
        return 8; // fail
    }

    if (u4) {
        return 9; // fail
    }

    return 0;
}



// Identical to chapter 11's compound_bitwise.c, but inspect assembly
int target_long_bitwise(void) {
    // bitwise compound operations on long integers
    long l1 = 71777214294589695l;  // 0x00ff_00ff_00ff_00ff
    long l2 = -4294967296;  // -2^32; upper 32 bits are 1, lower 32 bits are 0

    l1 &= l2;
    if (l1 != 71777214277877760l) {
        return 1; // fail
    }

    l2 |= 100l;
    if (l2 != -4294967196) {
        return 2;
    }

    l1 ^= -9223372036854775807l;
    if (l1 != -9151594822576898047l /* 0x80ff_00ff_0000_0001 */) {
        return 3;
    }

    // if rval is int, convert to common type
    l1 = 4611686018427387903l;  // 0x3fff_ffff_ffff_ffff
    int i = -1073741824;  // 0b1100....0, or 0xc000_0000
    // 1. sign-extend i to 64 bits; upper 32 bits are all 1s
    // 2. take bitwise AND of sign-extended value with l1
    // 3. result (stored in l1) is 0x3fff_ffff_c000_0000;
    //    upper bits match l1, lower bits match i
    l1 &= i;
    if (l1 != 4611686017353646080l) {
        return 4;
    }

    // if lval is int, convert to common type, perform operation, then convert back
    i = -2147483648l; // 0x8000_0000
    // check result and side effect
    // 1. sign extend 0x8000_0000 to 0xffff_ffff_8000_0000
    // 2. calculate 0xffff_ffff_8000_0000 | 0x00ff_00ff_00ff_00ff = 0xffff_ffff_80ff_00ff
    // 3. truncate to 0x80ff_00ff on assignment
    if ((i |= 71777214294589695l) != -2130771713) {
        return 5;
    }
    if (i != -2130771713) {
        return 6;
    }

    return 0; // success
}


// similar to chapter 11's compound_bitshift.c, but we inspect assembly
int target_long_bitshift(void) {
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

// similar to chapter 12's compound_bitwise.c, but we inspect assembly
int target_unsigned_bitwise(void) {
    unsigned long ul = 18446460386757245432ul; // 0xfffe_fdfc_fbfa_f9f8
    ul &= -1000; // make sure we sign-extend -1000 to unsigned long
    if (ul != 18446460386757244952ul /* 0xfffe_fdfc_fbfa_f818 */) {
        return 1; // fail
    }

    ul |= 4294967040u; // 0xffff_ff00 - make sure we zero-extend this to unsigned long

    if (ul != 18446460386824683288ul /* 0xfffe_fdfc_ffff_ff18 */) {
        return 2; // fail
    }

    // make sure that we convert result _back_ to type of lvalue,
    // and that we don't clobber nearby values (e.g. by trying to assign 8-byte)
    // result to four-byte ui variable
    int i = 123456;
    unsigned int ui = 4042322160u; // 0xf0f0_f0f0
    long l = -252645136; // 0xffff_ffff_f0f0_f0f0
    // 1. zero-extend ui to 8-bytes
    // 2. XOR w/ l, resulting in 0xffff_ffff_0000_0000
    // 3. truncate back to 4 bytes, resulting in 0
    // then check value of expression (i.e. value of ui)
    if (ui ^= l) {
        return 3; // fail
    }

    // check side effect (i.e. updating ui)
    if (ui) {
        return 4; // fail
    }
    // check neighbors
    if (i != 123456) {
        return 5;
    }
    if (l != -252645136) {
        return 6;
    }

    return 0; // success
}

// Identical to to chapter 12's compound_bitshift.c, but inspect assembly
int target_unsigned_bitshift(void) {

    // make sure we don't convert to common type before performing shift operation
    int i = -2;
    // don't convert i to common (unsigned) type; if we do, we'll use logical
    // instead of arithmetic shift, leading to wrong result
    i >>= 3u;
    if (i != -1) {
        return 1;
    }

    unsigned long ul = 18446744073709551615UL;  // 2^64 - 1
    ul <<= 44;                                  // 0 out lower 44 bits
    if (ul != 18446726481523507200ul) {
        return 2;  // fail
    }
    return 0;  // success
}

int main(void) {
    if (target_chars()) {
        return 1; // fail
    }

    if (target_long_bitwise()) {
        return 2; // fail
    }

    if (target_long_bitshift()) {
        return 3; // fail
    }

    if (target_unsigned_bitwise()) {
        return 4; // fail
    }

    if (target_unsigned_bitshift()) {
        return 5; // fail
    }

    return 0; // success
}