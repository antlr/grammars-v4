int main(void) {
    int a = 10;
    int b = 20;
    for (b = -20; b < 0; b = b + 1) {
        a = a - 1;
        if (a <= 0)
            break;
    }

    return a == 0 && b == -11;
}
