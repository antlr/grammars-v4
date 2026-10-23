int fib(int a);

int multiply_many_args(int a, int b, int c, int d, int e, int f, int g, int h);

int main(void) {
    int x = fib(4); // 3
    // at least until we implement optimizations, seven will have other values
    // adjacent to it in memory, which we'll push onto the stack when we pass it as an arg;
    // this tests that the caller will just look at 7 and not the junk bytes next to it
    int seven = 7;
    int eight = fib(6);
    int y = multiply_many_args(x, 2, 3, 4, 5, 6, seven, eight);
    if (x != 3) {
        return 1;
    }
    if (y != 589680) {
        return 2;
    }
    return x + (y % 256);
}