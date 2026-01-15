/* Test conversions from double to unsigned integer types */

unsigned int double_to_uint(double d) {
    return (unsigned int) d;
}

unsigned long double_to_ulong(double d) {
    return (unsigned long) d;
}

int main(void) {

    // try a double in the range of signed int;
    if (double_to_uint(10.9) != 10u) {
        return 1;
    }

    // now try a double in the range of unsigned int but not of int
    if (double_to_uint(2147483750.5) != 2147483750) {
        return 2;
    }

    // convert a double within the range of signed long,
    // so cvttsd2siq is already correct
    if (double_to_ulong(34359738368.5) != 34359738368ul) {
        return 3;
    }

    // now convert a double that's larger than LONG_MAX,
    // so we need to correct the results of cvttsd2siq
    if (double_to_ulong(3458764513821589504.0) != 3458764513821589504ul) {
        return 4;
    }

    return 0;

}