/* This is the same as valid/chars/char_arguments.c but split into two files */
int check_args(char a, signed char b, char c, unsigned char d, char e, char f, signed char g, char h);

int main(void) {
    char a = 5;
    signed char b = -12;
    char c = 117;
    unsigned char d = 254;
    char e = 1;
    char f = -20;
    signed char g = 60;
    char h = 100;


    return check_args(a, b, c, d, e, f, g, h);
}