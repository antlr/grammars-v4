#ifdef SUPPRESS_WARNINGS
#pragma GCC diagnostic ignored "-Wpointer-to-int-cast"
#pragma GCC diagnostic ignored "-Wint-to-pointer-cast"
#endif
/* Test explicit conversions to and from character types */

// conversions between character types
unsigned char char_to_uchar(char c) {
    return (unsigned char)c;
}

signed char char_to_schar(char c) {
    return (signed char)c;
}

char uchar_to_char(unsigned char u) {
    return (char)u;
}

char schar_to_char(signed char u) {
    return (char)u;
}

signed char uchar_to_schar(unsigned char u) {
    return (signed char)u;
}

unsigned char schar_to_uchar(signed char u) {
    return (unsigned char)u;
}

// char to other types
int char_to_int(char c) {
    return (int)c;
}

unsigned int char_to_uint(char c) {
    return (unsigned int)c;
}

long char_to_long(char c) {
    return (long)c;
}

unsigned long char_to_ulong(char c) {
    return (unsigned long)c;
}

double char_to_double(char c) {
    return (double)c;
}

// signed char to other types
int schar_to_int(signed char s) {
    return (int)s;
}

unsigned int schar_to_uint(signed char s) {
    return (unsigned int)s;
}

long schar_to_long(signed char s) {
    return (long)s;
}

unsigned long schar_to_ulong(signed char s) {
    return (unsigned long)s;
}

double schar_to_double(signed char s) {
    return (double)s;
}

// unsigned char to other types
int uchar_to_int(unsigned char u) {
    return (int)u;
}

unsigned int uchar_to_uint(unsigned char u) {
    return (unsigned int)u;
}

long uchar_to_long(unsigned char u) {
    return (long)u;
}

unsigned long uchar_to_ulong(unsigned char u) {
    return (unsigned long)u;
}

double uchar_to_double(unsigned char u) {
    return (double)u;
}

// other types to char
char int_to_char(int i) {
    return (char)i;
}

char uint_to_char(unsigned int u) {
    return (char)u;
}

char double_to_char(double d) {
    return (char)d;
}

// other types to signed char
signed char long_to_schar(long l) {
    return (signed char)l;
}

signed char ulong_to_schar(unsigned long l) {
    return (signed char)l;
}

// other types to unsigned char
unsigned char int_to_uchar(int i) {
    return (unsigned char)i;
}

unsigned char uint_to_uchar(unsigned int ui) {
    return (unsigned char)ui;
}

unsigned char long_to_uchar(long l) {
    return (unsigned char)l;
}

unsigned char ulong_to_uchar(unsigned long ul) {
    return (unsigned char)ul;
}

unsigned char double_to_uchar(double d) {
    return (unsigned char)d;
}

int main(void) {
    // char to other types
    char c = 127;

    if (char_to_uchar(c) != 127) {
        return 1;
    }

    if (char_to_int(c) != 127) {
        return 2;
    }

    if (char_to_ulong(c) != 127) {
        return 3;
    }

    // signed char to other types
    signed char sc = -10;
    if (schar_to_uchar(sc) != 246) {
        return 4;
    }
    if (schar_to_long(sc) != -10) {
        return 5;
    }

    if (schar_to_uint(sc) != 4294967286u) {
        return 6;
    }

    if (schar_to_double(sc) != -10.0) {
        return 7;
    }

    // unsigned char to other types
    unsigned char uc = 250;
    if (uchar_to_int(uc) != 250) {
        return 8;
    }

    if (uchar_to_long(uc) != 250) {
        return 9;
    }
    if (uchar_to_uint(uc) != 250) {
        return 10;
    }

    if (uchar_to_ulong(uc) != 250) {
        return 11;
    }

    if (uchar_to_double(uc) != 250.0) {
        return 12;
    }

    if (uchar_to_schar(uc) != -6) {
        return 13;
    }

    if (uchar_to_char(uc) != -6) {
        return 14;
    }

    // other types to char
    c = (char)-128;
    if (int_to_char(128) != c) {
        return 15;
    }

    c = (char)-6;
    if (uint_to_char(2147483898u) != c) {
        return 16;
    }

    c = (char)-2;
    if (double_to_char(-2.6) != c) {
        return 17;
    }

    // other types to schar
    if (long_to_schar(17592186044416l)) {  // should be null byte
        return 18;
    }

    sc = (signed char)-126;
    if (ulong_to_schar(9224497936761618562ul) != sc) {
        return 19;
    }

    // other types to uchar
    uc = (unsigned char)200;
    if (int_to_uchar(-1234488) != uc) {
        return 20;
    }
    if (uint_to_uchar(4293732808) != uc) {
        return 21;
    }

    if (long_to_uchar(-36283884951096l) != uc) {
        return 22;
    }

    if (ulong_to_uchar(9224497936761618632ul) != uc) {
        return 23;
    }

    if (double_to_uchar(200.99) != uc) {
        return 24;
    }

    // you can cast a pointer type to a char
    static long *null_ptr;
    char zero = (char)null_ptr;
    if (zero) {
        return 25;
    }

    // you can cast a char to a pointer type as long as it isn't misaligned
    c = 32;
    int *i = (int *)c;
    // conversion should round-trip
    if ((char)i != c) {
        return 26;
    }

    // truncate a constant that can't fit into a byte
    // to test rewrite rule for:
    //   movb $bigger_than_one_byte_constant, %dst
    if ((char)300 != (char)44) {
        return 27;
    }

    return 0;
}
