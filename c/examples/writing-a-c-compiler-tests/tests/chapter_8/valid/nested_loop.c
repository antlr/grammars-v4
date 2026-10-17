int main(void) {
    int acc = 0;
    int x = 100;
    while (x) {
        int y = 10;
        x = x - y;
        while (y) {
            acc = acc + 1;
            y = y - 1;
        }
    }
    return acc == 100 && x == 0;

}
