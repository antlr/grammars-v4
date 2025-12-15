/* Constant-folding tests for conversions from negative doubles to integer
 * types; couldn't test these before because we need copy prop to fully evaluate
 * them.
 * */

char target_to_char(void) {
    return (char)-126.5;
}

int target_to_int(void) {
    return (int)-5.9;
}

long target_to_long(void) {
    // nearest representable double is -9223372036854774784.0,
    // which will be converted to long int -9223372036854774784
    return (long)-9223372036854774783.1;
}

int main(void) {
    if (target_to_char() != -126) {
        return 1;
    }
    if (target_to_int() != -5) {
        return 2;
    }
    if (target_to_long() != -9223372036854774784l) {
        return 3;
    }
    return 0;
}