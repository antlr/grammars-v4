/* Test constant folding of casts from integer types to double
 * making sure the results are correctly rounded.
 * */

// can convert 32-bit ints to double exactly
double target_from_int(void) {
    return (double)1000;
}

// cast a double outside the range of int
// (See example in Chapter 13, "Converting an Unsigned Integer to a double")
double target_from_uint(void) {
    return (double)4294967290u;
}

// this value is exactly between two representable doubles;
// using ties-to-even, it should be converted to 4611686018427387904.0
// (From double-rounding example in Chapter 13, "Converting an Unsigned Integer
// to a double")
double target_from_long(void) {
    return (double)4611686018427388416l;
}

// convert a value outside the range of signed long
// (From double-rounding example in Chapter 13, "Converting an Unsigned Integer
// to a double")
double target_from_ulong(void) {
    return (double)9223372036854776833ul;
}

// same as target_from_int but cast is implicit;
// make sure we still constant fold it
double target_implicit(void) {
    return 1000;
}

int main(void) {
    if (target_from_int() != 1000.0) {
        return 1;
    }
    if (target_from_uint() != 4294967290.0) {
        return 2;
    }
    if (target_from_long() != 4611686018427387904.0) {
        return 3;
    }
    if (target_from_ulong() != 9223372036854777856.0) {
        return 4;

    }
    if (target_implicit() != 1000.0) {
        return 5;
    }
    return 0;
}