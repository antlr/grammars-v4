/* Test constant folding of all conversions from longer to shorter integer
 * types. For now, we just verify that the behavior is correct, without
 * inspecting the assembly code; we can't tell whether a truncate operation was
 * constant folded because it turns into a single 'mov' instruction either way.
 * The whole_pipeline/ folder has more robust tests for constant folding of
 * truncate operations - in these tests, constant-folding truncate enables
 * other optimizations.
 */

#ifdef SUPPRESS_WARNINGS
#ifdef __clang__
#pragma clang diagnostic ignored "-Wconstant-conversion"
#else
#pragma GCC diagnostic ignored "-Woverflow"
#endif
#endif

// truncate long
int long_to_int(void) {
    // 2^45 + 2^35 + 1234
    return (int)35218731828434l;
}

unsigned int long_to_uint(void) {
    // 2^45 + 2^35 + 1234
    return (unsigned int)35218731828434l;
}

char long_to_char(void) {
    // LONG_MAX
    return (char)9223372036854775807l;
}

signed char long_to_schar(void) {
    // 2^62 + 128
    return (signed char)4611686018427388032l;
}

unsigned char long_to_uchar(void) {
    // UINT_MAX
    return (unsigned char)4294967295UL;
}

// truncate unsigned long
int ulong_to_int(void) {
    // ULONG_MAX
    return (int)18446744073709551615UL;
}

unsigned int ulong_to_uint(void) {
    return (unsigned int)18446744073709551615UL;
}

char ulong_to_char(void) {
    return (char)4294967295UL;
}

signed char ulong_to_schar(void) {
    return (signed char)4611686018427388032ul;
}

unsigned char ulong_to_uchar(void) {
    // 2^63 + 255
    return (unsigned char)9223372036854776063ul;
}

// truncate int
char int_to_char(void) {
    return (char)1274;
}

signed char int_to_schar(void) {
    // INT_MAX
    return (signed char)2147483647;
}

unsigned char int_to_uchar(void) {
    return (unsigned char)1274;
}

// truncate unsigned int
char uint_to_char(void) {
    return (char)2147483901u;  // 2^31 + 253
}

signed char uint_to_schar(void) {
    return (signed char)2147483660u;  // 2^31 + 12
}

unsigned char uint_to_uchar(void) {
    return (unsigned char)2147483901u;
}

// same as uint_to_uchar but implicit cast
unsigned char implicit(void) {
    return 2147483901u;
}

int one = 1;
int six = 6;
int three = 3;
int one_twenty_eight = 128;

int main(void) {
    // truncate longs

    // 0x0000_2008_0000_04d2 --> 0x0000_04d2
    if (long_to_int() != 1234) {
        return 1;
    }
    if (long_to_uint() != 1234u) {
        return 2;
    }

    // 0x7fff_ffff_ffff_ffff --> 0xff
    if (long_to_char() != -one) {
        return 3;
    }

    // 0x4000_0000_0000_0080 --> 0x80
    if (long_to_schar() != -one_twenty_eight) {
        return 4;
    }

    // 0x0000_0000_ffff_ffff -> 0xff
    if (long_to_uchar() != 255) {
        return 5;
    }

    // truncate ulongs

    // 0xffff_ffff_ffff_ffff --> 0xffff_ffff
    if (ulong_to_int() != -one) {
        return 6;
    }
    if (ulong_to_uint() != 4294967295U) {
        return 7;
    }

    // 0x7fff_ffff_ffff_ffff --> 0xff
    if (ulong_to_char() != -one) {
        return 8;
    }

    // 0x4000_0000_0000_0080 --> 0x80
    if (ulong_to_schar() != -one_twenty_eight) {
        return 9;
    }

    // 0x0000_0000_ffff_ffff -> 0xff
    if (ulong_to_uchar() != 255) {
        return 10;
    }

    // truncate ints

    // 0x0000_04fa -> 0xfa
    if (int_to_char() != -six) {
        return 11;
    }

    // 0x7fff_ffff -> 0xff
    if (int_to_schar() != -one) {
        return 12;
    }

    // 0x0000_04fa -> 0xfa
    if (int_to_uchar() != 250) {
        return 13;
    }

    // truncate uints

    // 0x8000_00fd --> 0xfd
    if (uint_to_char() != -three) {
        return 14;
    }

    // 0x8000_000c -> 0x0c
    if (uint_to_schar() != 12) {
        return 15;
    }
    // 0x8000_00fd --> 0xfd
    if (uint_to_uchar() != 253) {
        return 16;
    }
    if (implicit() != 253) {
        return 17;
    }
    return 0;
}