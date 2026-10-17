/* Test conversions from signed integer types to double */
double int_to_double(int i) {
    return (double) i;
}

double long_to_double(long l) {
    return (double) l;
}
int main(void) {

    if (int_to_double(-100000) != -100000.0) {
        return 1;
    }

    if (long_to_double(-9007199254751227l) != -9007199254751228.0) {
        return 2;
    }

    // cast a constant to double to exercise rewrite rule for cvtsi2sd $const, dst
    double d = (double) 1152921504606846977l; // 2^60 + 1; nearest double is 2^60
    if (d != 1152921504606846976.0) {
        return 3;
    }

    return 0;
}