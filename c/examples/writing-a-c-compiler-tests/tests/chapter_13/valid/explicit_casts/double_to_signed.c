/* Test conversions from double to the signed integer types */

int double_to_int(double d) {
    return (int) d;
}

long double_to_long(double d) {
    return (long) d;
}

int main(void) {

    // when truncated, d will fit in a long
    // but not an int
    long l = double_to_long(2148429099.3);
    // should be truncated towards 0
    if (l != 2148429099l) {
        return 1;
    }

    int i = double_to_int(-200000.9999);
    // negative value should be truncated towards 0
    if (i != -200000) {
        return 2;
    }

    return 0;
}