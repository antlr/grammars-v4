long l = 4294967300l;

long *get_ptr(void) {
    return &l;
}
int main(void) {
    switch (*get_ptr()) {
        case 1:
            return 1;
        case 4: // l % 2^32
            return 2;
        case 4294967300l:
            return 0; // success
        case 18446744073709551600UL:
            return 3;
        default:
            return 4;
    }
}