// Test that we handle NaN correctly

int double_isnan(double d); // defined in tests/chapter_13/helper_libs/nan.c

// This should return zero, because all comparisons with NaN are false
int main(void) {
    static double zero = 0.0;
    double nan = 0.0 / zero; // make this constant-folding proof
    if (nan < 0.0 || nan == 0.0 || nan > 0.0 || nan <= 0.0 || nan >= 0.0)
        return 1;

    if (1 < nan || 1 == nan || 1 > nan || 1 <= nan || 1 >= nan)
        return 2;

    if (nan == nan)
        return 3;

    if (!(nan != nan)) { // != should evaluate to true
        return 4;
    }

    if (!double_isnan(nan)) {
        return 5;
    }

    if (!double_isnan(4 * nan)) {
        return 6;
    }

    if (!double_isnan(22e2 / nan)) {
        return 7;
    }

    if (!double_isnan(-nan)) {
        return 8;
    }

    // NaN should always evaluate to nonzero

    if (!nan) {
        return 9;
    }

    if (nan) {
    } else {
        return 10;
    }

    int nan_is_nonzero;
    for (nan_is_nonzero = 0; nan;) {
        nan_is_nonzero = 1;
        break;
    }
    if (!nan_is_nonzero) {
        return 11;
    }

    nan_is_nonzero = 0;
    while (nan) {
        nan_is_nonzero = 1;
        break;
    }
    if (!nan_is_nonzero) {
        return 12;
    }

    nan_is_nonzero = -1;
    do {
        nan_is_nonzero = nan_is_nonzero + 1;
        if (nan_is_nonzero) {
            break;
        }
    } while (nan);
    if (!nan_is_nonzero) {
        return 13;
    }

    nan_is_nonzero = nan ? 1 : 0;
    if (!nan_is_nonzero) {
        return 14;
    }

    return 0;
}