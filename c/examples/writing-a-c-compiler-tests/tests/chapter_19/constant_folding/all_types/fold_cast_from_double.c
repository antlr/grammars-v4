/* Test constant folding of casts from double to integer types,
 * making sure the results are correctly rounded.
 * */

char target_to_char(void) {
    return (char)126.5;
}

unsigned char target_to_uchar(void) {
    return (unsigned char)254.9;
}

int target_to_int(void) {
    return (int)5.9;
}

unsigned target_to_uint(void) {
    // constant in the range of uint but not int
    return (unsigned)2147483750.5;
}

long target_to_long(void) {
    // nearest representable double is 9223372036854774784.0,
    // which will be converted to long int 9223372036854774784
    return (long)9223372036854774783.1;
}

unsigned long target_to_ulong(void) {
    // constant in the range of ulong but not long
    return (unsigned long)13835058055282163712.5;
}

unsigned long target_implicit(void) {
    // same as target_to_ulong but cast is implicit; make sure we still constant fold it
    return 3458764513821589504.0;
}

int main(void) {
    if (target_to_char() != 126) {
        return 1;
    }
    if (target_to_uchar() != 254) {
        return 2;
    }
    if (target_to_int() != 5) {
        return 3;
    }
    if (target_to_uint() != 2147483750u) {
        return 4;
    }
    if (target_to_long() != 9223372036854774784l) {
        return 5;
    }
    if (target_to_ulong() != 13835058055282163712ul) {
        return 6;
    }
    if (target_implicit() != 3458764513821589504ul) {
        return 7;
    }
    return 0;
}