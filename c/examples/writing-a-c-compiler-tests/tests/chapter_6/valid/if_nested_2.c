int main(void) {
    int a = 0;
    int b = 1;
    if (a)
        b = 1;
    else if (~b)
        b = 2;
    return b;
}