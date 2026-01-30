/* Test that we can constant-fold zero- and sign-extensions from shorter to
 * longer ints, and conversions from one integer type to another of the same
 * size (e.g. long to unsigned long). We inspect the assembly for sign-extension
 * operations to make sure there are no movsx instructions, but not for
 * zero-extension or conversions between types of the same size because those
 * turn into a single 'mov' instruction whether they're sign extended or not.
 * We also can't test sign- or zero-extension of character types yet because
 * there are no constants of character type. The whole_pipeline/ folder has more
 * robust tests for constant folding of all these type conversions - we can test
 * them more thoroughly once other optimizations are enabled.
 */

long uint_to_long(void) {
    return (long)4294967295U;
}

unsigned long uint_to_ulong(void) {
    return (unsigned long)4294967295U;
}

/* These next two are target_* functions b/c they require sign extension */
unsigned long target_int_to_ulong(void) {
    return (unsigned long)2147483647;
}

long target_int_to_long(void) {
    return (long)1;
}

int uint_to_int(void) {
    // outside the range of int; will be negative
    return (int)4294967200U;
}

unsigned int int_to_uint(void) {
    return (unsigned)2147480000;
}

long ulong_to_long(void) {
    // outside the range of long; will be negative
    return (long)18446744073709551615UL;
}

unsigned long long_to_ulong(void) {
    return 2147483650l;
}

long implicit(void) {
    // same as ulong_to_long, but cast is implicit
    return 18446744073709551615UL;
}

long one = 1l;
long ninety_six = 96l;

int main(void) {
    if (uint_to_long() != 4294967295l) {
        return 1;
    }

    if (uint_to_ulong() != 4294967295ul) {
        return 2;
    }

    if (target_int_to_ulong() != 2147483647l) {
        return 3;
    }

    if (target_int_to_long() != 1l) {
        return 4;
    }

    if (uint_to_int() != -ninety_six) {
        return 5;
    }

    if (int_to_uint() != 2147480000u) {
        return 6;
    }

    if (ulong_to_long() != -one) {
        return 7;
    }

    if (long_to_ulong() != 2147483650ul) {
        return 8;
    }

    if (implicit() != -one) {
        return 9;
    }

    return 0;  // success
}