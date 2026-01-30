int non_zero(double d) {
    return !d;
}

double multiply_by_large_num(double d) {
    return d * 2e20;
}

int main(void) {

    /* Make sure subnormal numbers are not rounded to zero */
    double subnormal = 2.5e-320;

    /* Perform an operation on a subnormal number to produce a normal number */
    if (multiply_by_large_num(subnormal) != 4.99994433591341498562e-300) {
        return 2;
    }

    // subnormal is non-zero, so !subnormal should be zero
    return non_zero(subnormal);
}