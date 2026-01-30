int main(void) {
    int a = 0;
    if (a) {
        int b = 2;
        return b;
    } else {
        int c = 3;
        if (a < c) {
            return !a;
        } else {
            return 5;
        }
    }
    return a;
}