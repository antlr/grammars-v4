int fib(int n) {
    if (n == 0 || n == 1) {
        return n;
    } else {
        return fib(n - 1) + fib(n - 2);
    }
}

int multiply_many_args(int a, int b, int c, int d, int e, int f, int g, int h) {

    return a * b * c * d * e * f * fib(g) * fib(h);
}