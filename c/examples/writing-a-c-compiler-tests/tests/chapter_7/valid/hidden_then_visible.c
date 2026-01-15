int main(void) {
    int a = 2;
    int b;
    {
        a = -4;
        int a = 7;
        b = a + 1;
    }
    return b == 8 && a == -4;
}