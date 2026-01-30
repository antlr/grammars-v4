int main(void) {
    int x = 10;
    (x -= 1) ? (x /= 2) : 0;
    return x == 4;
}