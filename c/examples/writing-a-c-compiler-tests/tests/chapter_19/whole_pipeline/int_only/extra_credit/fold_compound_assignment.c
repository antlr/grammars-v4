/* Test that we can evaluate compound assignment expressions at compile time */

int target(void) {
    int v = -100;
    int w = 100;
    int x = 200;
    int y = 300;
    int z = 400;

    v += 10;
    w -= 20;
    x *= 30;
    y /= 100;
    // include chained compound assignment
    z %= y += 6;

    if (v == -90 && w == 80 && x == 6000 && y == 9 && z == 4) {
        return 0; // success
    }

    return 1; //fail
}

int main(void) {
    return target();
}