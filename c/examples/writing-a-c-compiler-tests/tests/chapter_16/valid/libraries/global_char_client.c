/* Test that we can access global objects of character type in multiple translation units */

extern char c;
extern unsigned char uc;
extern signed char sc;

int update_global_chars(void);

int main(void) {
    // check initial values
    if (c != 100) {
        return 1;
    }

    if (uc != 250) {
        return 2;
    }

    if (sc != 0) {
        return 3;
    }

    update_global_chars();

    // check updated values
    if (c != 110) {
        return 4;
    }

    if (uc != 4) {
        return 5;
    }

    if (sc != -10) {
        return 6;
    }

    return 0;
}
