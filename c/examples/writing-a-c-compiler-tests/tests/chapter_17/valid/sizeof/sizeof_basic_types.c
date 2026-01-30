/* Make sure we can get the size of all basic type */

int main(void) {
    if (sizeof(char) != 1) {
        return 1;
    }

    if (sizeof(signed char) != 1) {
        return 2;
    }

    if (sizeof(unsigned char) != 1) {
        return 3;
    }

    if (sizeof(int) != 4) {
        return 4;
    }
    if (sizeof(unsigned int) != 4) {
        return 5;
    }

    if (sizeof(long) != 8) {
        return 6;
    }
    if (sizeof(unsigned long) != 8) {
        return 7;
    }

    if (sizeof(double) != 8) {
        return 8;
    }

    return 0;
}