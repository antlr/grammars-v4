int main(void) {
    int a = 250;
    int b = 200;
    int c = 100;
    int d = 75;
    int e = 50;
    int f = 25;
    int g = 10;
    int h = 1;
    int j = 0;
    int x = 0;
    x = a &= b *= c |= d = e ^= f += g >>= h <<= j = 1;
    return (a == 40 && b == 21800 && c == 109 && d == 41 && e == 41 &&
            f == 27 && g == 2 && h == 2 && j == 1 && x == 40);
}