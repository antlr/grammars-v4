int main(void) {
    int a = 1;
    int b = 0;
    a = 3 * (b = a);
    return a + b;
}