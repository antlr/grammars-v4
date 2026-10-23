/* Evaluate ++/-- with character types; make sure we handle integer promotions correctly */
int target(void) {
    signed char s = -127;
    signed char s2 = --s;
    signed char s3 = s--;

    unsigned char uc1 = 255;
    unsigned char uc2 = uc1++;
    unsigned char uc3 = ++uc1;

    if (!(s == 127 && s2 == -128 && s3 == -128)) {
        return 1; // fail
    }

    if (!(uc1 == 1 && uc2 == 255 && uc3 == 1)) {
        return 2; // fail
    }

    return 0; // success
}

int main(void) {
    return target();

}