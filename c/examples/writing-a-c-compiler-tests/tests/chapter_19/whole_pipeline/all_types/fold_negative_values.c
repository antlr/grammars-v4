/* Test constant folding with negative numbers (including double and long);
 * we couldn't test this in the constant-folding stage because it requires
 * copy propagation.
 * */

/* long tests */

/* similar to int-only remainder_test but with long instead of int */
long target_remainder_test(void) {
    // same expression as in chapter_11/valid/long_expressions/arithmetic_ops.c
    // but constant-foldable
    return -8589934585l % 4294967290l;
}

long target_long_subtraction(void) {
    // same expression as in chapter_11/valid/long_expressions/arithmetic_ops.c
    // but constant-foldable
    return -4294967290l - 90l;
}

long target_long_division(void) {
    // same expression as in chapter_11/valid/long_expressions/arithmetic_ops.c
    // but constant-foldable and w/ first operand negated
    return (-4294967290l / 128l);
}

long target_long_complement(void) {
    return ~-9223372036854775807l;
}

/* double tests */
double target_double_add(void) {
    // Because the magnitude of -1.2345e60 is so large,
    // adding one to it doesn't change its value
    // (same as target_add in
    // tests/chapter_19/constant_folding/all_types/fold_double.c with one
    // operand negated)
    return -1.2345e60 + 1.;
}

double target_double_sub(void) {
    // calculate the difference between two very close
    // subnormal numbers (same as target_sub in
    // tests/chapter_19/constant_folding/all_types/fold_double.c
    // with operands negated)
    return -5.85543871245623688067e-311 - -5.85543871245574281503e-311;
}

double target_double_div(void) {
    // same as target_div in
    // tests/chapter_19/constant_folding/all_types/fold_double.c
    // with one operand negated
    return -1100.5 / 5000.;
}

int main(void) {
    // long tests
    if (target_remainder_test() != -5l) {
        return 1;  // fail
    }
    if (target_long_subtraction() != -4294967380l) {
        return 2;  // fail
    }
    if (target_long_division() != -33554431l) {
        return 3;  // fail
    }
    if (target_long_complement() != 9223372036854775806l) {
        return 4;  // fail
    }
    // double tests
    if (target_double_add() != -1.2345e60) {
        return 5;  // fail
    }
    if (target_double_sub() != -5e-324) {
        return 6;  // fail
    }
    if (target_double_div() != -0.2201) {
        return 7;  // fail
    }
    return 0;
}