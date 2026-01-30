int main(void) {
    int x = 5;
    int acc = 0;
    while (x >= 0) {
        int i = x;
        while (i <= 10) {
            i = i + 1;
            if (i % 2)
                continue;
            acc = acc + 1;
        }
        x = x - 1;
    }
    return acc;
}
