/* Test constant folding of sign extension, zero extension, and truncation.
 * We couldn't test this thoroughly during the constant folding phase because
 * we hadn't implemented copy propagation yet.
 * */

#ifdef SUPPRESS_WARNINGS
#ifdef __clang__
#pragma clang diagnostic ignored "-Wconstant-conversion"
#else
#pragma GCC diagnostic ignored "-Woverflow"
#endif
#endif

/* Sign extension */

// Test sign-extension from int to long
// Make sure we propagate converted value, rather than
// original value, into later expression
long target_extend_int_to_long(void) {
    int i = -1000;
    long l = (long)i;
    return (l - 72057594037927936l) / 3l;  // result is outside the range of int
}

// Test sign-extension from int to ulong
// same idea as above
unsigned long target_extend_int_to_ulong(void) {
    int i = -1000;
    unsigned long u = (unsigned long)i;
    return u % 50ul;
}

/* Zero extension */
long target_extend_uint_to_long(void) {
    unsigned int u = 2147483648u;  // 2^31
    long l = (long)u;
    // make sure it's positive
    if (l < 0) {
        return 0;  // fail
    }
    return l % 7l;
}

unsigned long target_extend_uint_to_ulong(void) {
    unsigned int u = 4294967295U;
    unsigned long l = (unsigned long)u;
    return (l == 4294967295Ul);
}

/* Truncation */

// Test truncation from long to int
// make sure we're actually performing truncation (as opposed to,
// say, just storing ints as 64-bit values internally, then making truncation a
// no-op or zeroing out upper bytes regardless of sign)
int target_truncate_long_to_int(void) {
    long l = 9223372036854775807l;         // LONG_MAX
    int i = (int)l;                        // -1
    long l2 = -9223372036854775807l - 1l;  // LONG_MIN
    int i2 = (int)l2;                      // 0
    // make sure we propagate truncated value (0) and not original value
    // (nonzero)
    if (i2) {  // eliminate this
        return 0;
    }
    // make sure we propagate truncated value
    // if we use original value, result of division will be different
    // even if you only look at lower 32 bits
    return 20 / i;
}

// Test truncation from long to int
// same idea as above
unsigned int target_truncate_long_to_uint(void) {
    long l = -9223372032559808513l;  // LONG_MIN + UINT_MAX
    unsigned int u = (unsigned)l;    // UINT_MAX
    if (u - 4294967295U) {           // eliminate this
        return 0;
    }
    return u / 20;
}

// Test truncation from unsigned long to int
int target_truncate_ulong_to_int(void) {
    unsigned long ul = 18446744073709551615UL;  //  ULONG_MAX
    int i = (int)ul;                            // -1
    unsigned long ul2 = 9223372039002259456ul;  // 2^63 + 2^31
    int i2 = (int)ul2;                          // INT_MIN
    if (i2 >= 0) {                              // eliminate this
        return 0;
    }
    return 10 / i;  // -10
}

// Test truncation from unsigned long to unsigned int
unsigned int target_truncate_ulong_to_uint(void) {
    unsigned long ul = 18446744073709551615UL;  // ULONG_MAX
    unsigned int u = (unsigned int)ul;          // UINT_MAX
    return u / 20;
}

/* Conversions to/from character types.
 * There are no constants of character type, and chars are promoted
 * to int before almost every operation, so we can't test truncation and
 * extension separately
 * */

// Test truncation from int to char/signed char, and sign-extension
// from char/signed char to int
// make sure we're actually performing truncation/extension (as opposed to,
// say, just treating chars as 32-bit ints and making extension/truncation a
// no-op)
int target_char_int_conversion(void) {
    // convert a wide range of ints to chars
    int i = 257;
    char c = i;
    i = 255;
    char c2 = i;
    i = 2147483647;  // INT_MAX
    signed char c3 = i;
    i = -2147483647 - 1;  // INT_MIN
    char c4 = i;
    i = -129;  // all bits set except bit 128 - need to zero out all upper bits
               // when we convert this back to int
    signed char c5 = i;
    i = 128;  // only bit 128 is set - need to sign-extend to all upper bites
              // when we convert this back to int
    char c6 = i;
    // we'll convert these chars back to ints implicitly
    // as part of usual arithmetic conversions
    // for !=
    if (c != 1) {
        return 1;  // fail
    }
    if (c2 != -1) {
        return 2;  // fail
    }
    if (c3 != -1) {
        return 3;  // fail
    }
    if (c4 != 0) {
        return 4;  // fail
    }
    if (c5 != 127) {
        return 5;  // fail
    }
    if (c6 != -128) {
        return 6;  // fail
    }
    return 0;  // success
}

int target_uchar_int_conversion(void) {
    int i = 767;
    unsigned char uc1 = i;  // 255
    i = 512;
    unsigned char uc2 = i;  // 0
    i = -2147483647;        // INT_MIN + 1
    unsigned char uc3 = i;  // 1
    i = -2147483647 + 127;  // INT_MIN + 128
    unsigned char uc4 = i;  // 128

    // we'll implicitly zero-extend these unsigned chars back to ints
    // for comparisons
    if (uc1 != 255) {
        return 1;  // fail
    }
    if (uc2) {
        return 2;  // fail
    }
    if (uc3 != 1) {
        return 3;  // fail
    }
    if (uc4 != 128) {
        return 1;  // fail
    }
    return 0;  // success
}

int target_char_uint_conversion(void) {
    char c = 2148532223u;              // 2^30 + 2^20 - 1, truncates to -1
    signed char c2 = 2147483775u;      // 2^31 + 127, truncates to 127
    unsigned int u = (unsigned int)c;  // UINT_MAX
    if (u != 4294967295U) {
        return 1;  // fail
    }
    u = (unsigned int)c2;
    if (u != 127u) {
        return 2;  // fail
    }
    return 0;
}

int target_uchar_uint_conversion(void) {
    unsigned char uc = 2148532223u;  // 2^30 + 2^20 - 1, truncates to 255
    unsigned int ui = (unsigned int)uc;
    if (ui != 255u) {
        return 1;  // fail
    }
    return 0;
}

int target_char_long_conversion(void) {
    long l = 3377699720528001l;  // 2^51 + 2^50 + 129
    char c = l;                  // truncates to -127
    l = 9223372036854775807l;    // LONG_MAX
    char c2 = l;                 // -1
    l = 2147483648l + 127l;      // 2^32 + 127
    signed char c3 = l;          // 127
    l = -2147483647l - 1l;       // INT_MIN (as a long)
    char c4 = l;                 // 0
    l = 2147483648l + 128l;
    signed char c5 = l;  // -128
    // we'll convert these chars back to ints implicitly
    // as part of usual arithmetic conversions
    // for !=
    if (c != -127l) {
        return 1;  // fail
    }
    if (c2 != -1l) {
        return 2;  // fail
    }
    if (c3 != 127l) {
        return 3;   // fail
    }
    if (c4) {
        return 4;   // fail
    }
    if (c5 != -128l) {
        return 5;   // fail
    }
    return 0;  // success
}

int target_uchar_long_conversion(void) {
    long l = 255l + 4294967296l;
    unsigned char uc1 = l;            // 255
    l = 36028798092705792l;           // 2^55 + 2^30
    unsigned char uc2 = l;            // 0
    l = -9223372036854775807l;        // LONG_MIN + 1
    unsigned char uc3 = l;            // 1
    l = -9223372036854775807l + 127;  // LONG_MIN + 128
    unsigned char uc4 = l;            // 128

    // we'll implicitly zero-extend these unsigned chars back to ints
    // for comparisons
    if (uc1 != 255) {
        return 1;  // fail
    }
    if (uc2) {
        return 2;  // fail
    }
    if (uc3 != 1) {
        return 3;  // fail
    }
    if (uc4 != 128) {
        return 1;  // fail
    }
    return 0;  // success
}

int target_char_ulong_conversion(void) {
    char c = 9223373136366403583ul;          // 2^63 + 2^40 - 1, truncates to -1
    signed char c2 = 9223372036854775935ul;  // 2^63 + 127, truncates to 127
    unsigned long ul = (unsigned long)c;     // ULONG_MAX
    if (ul != 18446744073709551615UL) {
        return 1;  // fail
    }
    ul = (unsigned long)c2;
    if (ul != 127ul) {
        return 2;  // fail
    }
    return 0;
}

int target_uchar_ulong_conversion(void) {
    unsigned char uc =
        9223372037929566207ul;  // 2^63 + 2^30 + 2^20 - 1, truncates to 255
    unsigned int ui = (unsigned int)uc;
    if (ui != 255u) {
        return 1;  // fail
    }
    return 0;
}
int main(void) {
    if (target_extend_int_to_long() != -24019198012642978l) {
        return 1;  // fail
    }
    if (target_extend_int_to_ulong() != 16ul) {
        return 2;  // fail
    }
    if (target_extend_uint_to_long() != 2l) {
        return 3;  // fail
    }
    if (target_extend_uint_to_ulong() != 1ul) {
        return 4;  // fail
    }
    if (target_truncate_long_to_int() != -20) {
        return 5;  // fail
    }
    if (target_truncate_long_to_uint() != 214748364u) {
        return 6;  // fail
    }
    if (target_truncate_ulong_to_int() != -10) {
        return 7;  // fail
    }
    if (target_truncate_ulong_to_uint() != 214748364u) {
        return 8;  // fail
    }
    if (target_char_int_conversion()) {
        return 9;  // fail
    }
    if (target_uchar_int_conversion()) {
        return 10;  // fail
    }
    if (target_char_uint_conversion()) {
        return 11;  // fail
    }
    if (target_uchar_uint_conversion()) {
        return 12;  // fail
    }
    if (target_char_long_conversion()) {
        return 13;  // fail
    }
    if (target_uchar_long_conversion()) {
        return 14;  // fail
    }
    if (target_char_ulong_conversion()) {
        return 15;  // fail
    }
    if (target_uchar_ulong_conversion()) {
        return 16;  // fail
    }
    return 0;
}