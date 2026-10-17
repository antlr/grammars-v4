/* Test that we can propagate all arithmetic types, including doubles,
 * long and unsigned integers, and characters.
 * */

#ifdef SUPPRESS_WARNINGS
#ifdef __clang__
#pragma clang diagnostic ignored "-Wconstant-conversion"
#else
#pragma GCC diagnostic ignored "-Woverflow"
#endif
#endif

int target(void) {
    // propagate doubles
    double d = 1500.0;
    double d2 = d;

    int sum = (int)(d + d2);  // 3000

    // propagate chars
    char c = 250;  // will be converted to -6
    char c2 = c;
    sum = sum + (c2 + c);  // 2988

    // propagate unsigned char
    unsigned char uc = -1;  // will be converted to 255
    unsigned char uc2 = uc;
    sum = sum + uc + uc2;  // 3498

    // propagate unsigned long
    unsigned long ul = 18446744073709551615UL;  // ULONG_MAX
    unsigned long ul2 = ul + 3ul;               // wraps around to 2
    sum = sum + ul2;                            // 3500

    return sum;  // rewrite as "return 3500"
}

int main(void) {
    return target() == 3500;
}