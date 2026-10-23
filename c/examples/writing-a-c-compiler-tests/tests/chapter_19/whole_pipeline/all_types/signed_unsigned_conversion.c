/* Test constant-folding of conversions between signed and unsigned types
 * of the same size, allowing for further copy propagation
 * */

unsigned int target_int_to_uint(void) {
    int i = -1;
    // after constant folding this cast, we can propagate the value of u
    // into the return statement
    unsigned int u = (unsigned)i;
    return u / 10u;
}

int target_uint_to_int(void) {
    unsigned int u = 4294967295U;
    // after constant folding this cast, we can propagate the value of i
    // into the return statement
    int i = (int)u;  // -1;
    return (i + 1) ? 0 : i * 2;
}

long target_ulong_to_long(void) {
    unsigned long ul = 9223372036854775900ul;
    // after constant folding this cast, we can propagate the value of l
    // into the return statement
    signed long l = (long)ul;
    return l / 4;
}

unsigned long target_long_to_ulong(void) {
    long l = -200l;
    unsigned long ul = (unsigned long)l;
    return ul / 10;
}

int main(void) {
    if (target_int_to_uint() != 429496729u) {
        return 1;  // fail
    }
    if (target_uint_to_int() != -2) {
        return 2;  // fail
    }
    if (target_ulong_to_long() != -2305843009213693929) {
        return 3;  // fail
    }
    if (target_long_to_ulong() != 1844674407370955141ul) {
        return 4;  // fail
    }

    return 0;  // success
}