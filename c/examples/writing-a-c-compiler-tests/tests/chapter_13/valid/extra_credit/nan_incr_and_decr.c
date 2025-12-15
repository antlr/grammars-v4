// Test that we handle NaN correctly for increments and decrements
// All increments and decrements on NaN are also NaN

int double_isnan(double d); // defined in tests/chapter_13/helper_libs/nan.c

int main(void) {
    static double zero = 0.0;
    double nan = 0.0 / zero; // make this constant-folding proof

    if (!double_isnan(++nan)) {
        return 1;
    }

    if (!double_isnan(--nan)) {
        return 2;
    }

    if (!double_isnan(nan++)) {
        return 3;
    }

    if (!double_isnan(nan--)) {
        return 4;
    }

    return 0;
}