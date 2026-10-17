int main(void) {
    int a = 1;
    int b = 0;
    a ? (b = 1) : (b = 2);
    return b;
}