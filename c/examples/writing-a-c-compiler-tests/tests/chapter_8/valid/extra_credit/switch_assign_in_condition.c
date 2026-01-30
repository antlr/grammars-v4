int main(void) {
    int a = 0;
    switch (a = 1) {
        case 0:
            return 10;
        case 1:
            a = a * 2;
            break;
        default:
            a = 99;
    }
    return a;
}