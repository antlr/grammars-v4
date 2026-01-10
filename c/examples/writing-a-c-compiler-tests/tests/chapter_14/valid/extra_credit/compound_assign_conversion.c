// Test compound assignment through pointers involving type conversions

int main(void) {
    // lval is pointer
    double d = 5.0;
    double *d_ptr = &d;
    // convert 1000 to double
    *d_ptr *= 1000u;
    if (d != 5000.0) {
        return 1; // fail
    }
    int i = -50;
    int *i_ptr = &i;
    // convert *i_ptr to unsigned, perform operation, then convert back
    *i_ptr %= 4294967200U;
    if (*i_ptr != 46) {
        return 2; // fail
    }

    // rval is pointer
    unsigned int ui = 4294967295U; // 2^32 - 1
    ui /= *d_ptr;
    // convert ui to double, perform operation, and convert back
    if (ui != 858993u) {
        return 3; // fail
    }

    // both operands are pointers
    i = -10;
    unsigned long ul = 9223372036854775807ul; // 2^63 - 1
    unsigned long *ul_ptr = &ul;
    // convert i to common type (ul), perform operation, then
    // convert back to int
    *i_ptr -= *ul_ptr;
    if (i != -9) {
        return 4; // fail
    }

    // check neighbors
    if (ul != 9223372036854775807ul) {
        return 5; // fail
    }

    return 0;
}