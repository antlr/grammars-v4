int main(void) {
    int a = 10;
    // test that || is higher precedence than ?
    return a || 0 ? 20 : 0;
}