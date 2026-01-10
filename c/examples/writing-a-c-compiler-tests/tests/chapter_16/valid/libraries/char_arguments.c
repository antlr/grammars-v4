/* This is the same as valid/chars/char_arguments.c but split into two files */

/* Test that we can pass arguments and return values of character type */
int check_args(char a, signed char b, char c, unsigned char d, char e, char f, signed char g, char h) {
    char expected_a = 5;
    signed char expected_b = -12;
    char expected_c = 117;
    unsigned char expected_d = 254;
    char expected_e = 1;
    char expected_f = -20;
    signed char expected_g = 60;
    char expected_h = 100;

    if (expected_a != a) {
     return 1;
    }

    if (expected_b != b) {
     return 2;
    }

    if (expected_c != c) {
     return 3;
    }

    if (expected_d != d) {
     return 4;
    }

    if (expected_e != e) {
     return 5;
    }

    if (expected_f != f) {
     return 6;
    }

    if (expected_g != g) {
     return 7;
    }

    if (expected_h != h) {
     return 8;
    }

    return 0;
}
