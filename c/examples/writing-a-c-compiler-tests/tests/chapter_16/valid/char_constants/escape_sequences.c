/* Make sure we parse escape sequences to the correct value */
int main(void) {
    if ('\?' != 63) {
        return 1;
    }
    if ('\"' != 34) {
        return 2;
    }
    if ('\'' != 39) {
        return 3;
    }
    if ('\\' != 92) {
        return 4;
    }
    if ('\a' != 7) {
        return 5;
    }
    if ('\b' != 8) {
        return 6;
    }
    if ('\f' != 12) {
        return 7;
    }
    if ('\n' != 10) {
        return 8;
    }
    if ('\r' != 13) {
        return 9;
    }
    if ('\t' != 9) {
        return 10;
    }
    if ('\v' != 11) {
        return 11;
    }
    return 0;
}