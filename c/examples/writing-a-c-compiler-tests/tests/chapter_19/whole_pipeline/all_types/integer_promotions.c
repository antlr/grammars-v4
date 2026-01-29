/* Make sure we promote characters to integers before constant folding */

int target(void) {
    char c1 = 120;
    char c2 = 3;
    // if this weren't promoted, c1 + c1 would overflow, causing undefined behavior
    // if we had c1 + c1 wrap around to -16, this would result in -5
    // but because we promote results to ints, this is 240 / 3, or 80
    char c3 = (c1 + c1) / c2;

    unsigned char uc1 = 200;
    unsigned char uc2 = 12;
    // if we didn't perform integer promotions, uc1 + uc1 would wrap around
    // to 144 and the result would be 12. With promotions, this is 400/12, or 33.
    unsigned char uc3 = (uc1 + uc1) / uc2;
    if (c3 != 80) {
        return 1; // fail
    }
    if (uc3 != 33) {
        return 2; // fail
    }
    return 0;
}

int main(void) {
    return target();
}