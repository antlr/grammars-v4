/* Test casts from char * to other character pointer types */

int main(void) {
    char *c = "This is a string!";
    unsigned char *uc = (unsigned char *)c;
    if (uc[3] != 's') {
        return 1;
    }
    signed char *sc = (signed char *)c;
    if (sc[3] != 's'){
            return 2;
        }
    return 0;
}