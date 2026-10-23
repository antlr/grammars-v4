/* Test chaining multiple explicit casts together*/

// start with a global variable so we can't optimize away casts in Part III
unsigned int ui = 4294967200u;  // 2^32 - 96

int main(void) {
    /* 1. convert  ui to an unsigned char with value 256 - 96
     * 2. convert it back to an unsigned int, which preserves its value.
     */
    ui = (unsigned int)(unsigned char)ui;
    if (ui != 160) {
        return 1;
    }

    /* 1. convert  ui to a signed char with value - 96
     * 2. convert it to a signed int, preserving it svalue
     */
    int i = (int)(signed char)ui;
    if (i != -96) {
        return 2;
    }

    return 0;
}