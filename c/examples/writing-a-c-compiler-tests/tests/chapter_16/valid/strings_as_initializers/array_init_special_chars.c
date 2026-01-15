/* Test that we can handle escape sequences in string literals */
int main(void) {
    // a mix of escaped and unescaped special characters
    char special[6] = "\a\b\n	";

    if (special[0] != '\a') {
        return 1;
    }

    if (special[1] != '\b') {
        return 2;
    }

    if (special[2] != '\n') {
        return 3;
    }
    if (special[3] != '\v') {
        return 4;
    }
    if (special[4] != '\f') {
        return 5;
    }

    if (special[5] != '\t') {
        return 6;
    }

    return 0;
}