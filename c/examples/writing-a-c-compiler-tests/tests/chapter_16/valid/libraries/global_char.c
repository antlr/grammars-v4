char c = 100;
unsigned char uc = 250;
signed char sc = 0;

int update_global_chars(void) {
    c = c + 10;
    uc = uc + 10; // wraps around
    sc = sc - 10;
    return 0;
}