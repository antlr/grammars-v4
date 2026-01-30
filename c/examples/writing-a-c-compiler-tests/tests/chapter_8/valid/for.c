int main(void) {
    int a = 12345;
    int i;

    for (i = 5; i >= 0; i = i - 1)
        a = a / 3;

    return a;
}
