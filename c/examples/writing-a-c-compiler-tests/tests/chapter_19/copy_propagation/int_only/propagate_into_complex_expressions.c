/* Test that we can propagate values from copies
 * into unary expressions, binary expressions,
 * and conditional jumps.
 * */
int target(void) {
    int x = 100;
    int y = -x * 3 + 300;
    return (y ? x % 3 : x / 4);
}

int main(void) {
    return target() == 25;
}