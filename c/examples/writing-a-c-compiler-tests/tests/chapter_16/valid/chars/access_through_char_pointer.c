/* Test that we can read/write any object through a pointer to a character type
 * (this is an exception to the effective typing/strict aliasing rules)
 * This is all implementation-defined since it depends on exact size/layout of different types
 */

int main(void) {

    // inspect the four bytes of an int
    int x = 100;
    char *byte_ptr = (char *) &x;

    if (byte_ptr[0] != 100) {
        return 1;
    }

    if (byte_ptr[1] || byte_ptr[2] || byte_ptr[3]) {
        return 2;
    }

    // now inspect a double -- only upper bit should be set
    double d = -0.0; // 0x8000_0000_0000_0000
    byte_ptr = (char *) &d;
    if (byte_ptr[7] != -128) {
        return 3;
    }

    for (int i = 0; i < 7; i = i + 1) {
        if (byte_ptr[i]) {
            return 4;
        }
    }

    // finally, let's look at an array
    unsigned int array[3][2][1] = {
        {{-1}, {-1}},
        {{-1}, {-1}},
        {{4294901760u}} // 0xffff_0000
    };
    byte_ptr = (char *) array;
    byte_ptr = byte_ptr + 16; // each row is 8 bytes since it has 2 ints
    if (byte_ptr[0] || byte_ptr[1]) {
        return 5;
    }

    if (byte_ptr[2] != -1) {
        return 6;
    }

    if (byte_ptr[3] != -1) {
        return 7;
    }

    return 0;
}