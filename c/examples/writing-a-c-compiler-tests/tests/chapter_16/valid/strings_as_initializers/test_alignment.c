// Test that any char array variables larger than 16 bytes are 16-byte aligned

int check_aligment(char *c) {
    unsigned long l = (unsigned long)c;
    return (l % 16 == 0);  // return 1 on success, 0 on failure
}

// define some static arrays that are >= 16 bytes
static signed char flat_static[16] = "x";
static unsigned char nested_static[3][4][2] = {{"a"}, {"b"}};

int main(void) {
    // define some automatic arrays that larger than 16 bytes
    char flat_auto[22];
    char nested_auto[10][3];

    if (!check_aligment((char *)flat_static)) {
        return 1;
    }

    if (!check_aligment((char *)nested_static)) {
        return 2;
    }

    if (!check_aligment((char *)flat_auto)) {
        return 3;
    }

    if (!check_aligment((char *)nested_auto)) {
        return 4;
    }

    return 0;
}