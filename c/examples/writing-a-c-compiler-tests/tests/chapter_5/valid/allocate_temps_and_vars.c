int main(void) {
    int a = 2147483646;
    int b = 0;
    int c = a / 6 + !b;
    return c * 2 == a - 1431655762;
}