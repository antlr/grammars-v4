/* Test that we can constant-fold !, -, and ~ expressions. */

int target_negate(void) {
    return -3;
}

int target_negate_zero(void) {
    return -0;
}

int target_not(void) {
    return !1024;
}

int target_not_zero(void) {
    return !0;
}

int target_complement(void) {
    return ~1;
}

int three = 3;
int two = 2;

int main(void) {

    if (target_negate() != -three)
        return 1;

    if (target_negate_zero() != 0) {
        return 2;
    }

    if (target_not() != 0) {
        return 3;
    }

    if (target_not_zero() != 1) {
        return 4;
    }

    if (target_complement() != -two) {
        return 5;
    }

    return 0;  // success
}