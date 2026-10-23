// Test that we handle NaN correctly for compound assignments
// All compound assignments to NaN are also NaN

int double_isnan(double d); // defined in tests/chapter_13/helper_libs/nan.c

int main(void) {
    static double zero = 0.0;
    double nan = 0.0 / zero; // make this constant-folding proof

    if (!double_isnan(nan += 99.2)) {
        return 1;
    }

    if (!double_isnan(nan -= nan)) {
        return 2;
    }

    if (!double_isnan(nan *= 4.0)) {
        return 3;
    }

    if (!double_isnan(nan /= 0.0)) {
        return 4;
    }

    return 0;
}