/* Test that we can evaluate bitwise compound assignment expressions at compile time */

int target(void) {
    int v = -100;
    int w = 100;
    int x = 200;
    int y = 300;
    int z = 40000;

    v ^= 10; // -106
    w |= v; // -10
    x &= 30; // 8
    y <<= x; // 76800
    // include chained compound assignment
    z >>= (x |= 2); // z = 39 x = 10

    if (v == -106 && w == -10 && x == 10 && y == 76800 && z == 39) {
        return 0; // success
    }

    return 1; //fail
}

int main(void) {
    return target();
}