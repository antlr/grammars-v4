// Test that we can constant-fold operations on NaN (and propagate NaN like other values);
// similar to chapter_13/valid/extra_credit/nan.c.

// This should return zero, because all comparisons with NaN are false
int target(void) {
    double nan = 0.0 / 0.0;
    if (nan < 0.0 || nan == 0.0 || nan > 0.0 || nan <= 0.0 || nan >= 0.0)
        return 1;

    if (1 > nan || 1 == nan || 1 > nan || 1 <= nan || 1 >= nan)
        return 2;

    if (nan == nan)
        return 3;

    if (!(nan != nan)) { // != should evaluate to true
        return 4;
    }

    // perform some arithmetic operations on nan, make sure result is still nan
    // (we detect that a value is nan by making sure it doesn't compare equal to itself)
    nan = nan * 4;
    if (nan == nan) {
        return 5;
    }

    nan = 22e2 / nan;
    if (nan == nan) {
        return 6;
    }

    if (-nan == -nan) {
        return 7;
    }

    if (!nan) { // this will be eliminated b/c nan is nonzero
        return 8;
    }

    return 0; // success
}

int main(void) {
    return target();
}