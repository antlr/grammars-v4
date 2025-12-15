int foo(int a, int b, int c, int d, int e, int f, int g, int h) {
    return (a == 1 && b == 2 && c == 3 && d == 4 && e == 5
            && f == 6 && g == 7 && h == 8);
}

int main(void) {
    return foo(1, 2, 3, 4, 5, 6, 7, 8);
}