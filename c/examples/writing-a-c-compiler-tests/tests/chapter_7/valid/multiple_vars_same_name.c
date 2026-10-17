int main(void) {
    int a = 0;
    {
        int b = 4;
        a = b;
    }
    {
        int b = 2;
        a = a - b;
    }
    return a;
}