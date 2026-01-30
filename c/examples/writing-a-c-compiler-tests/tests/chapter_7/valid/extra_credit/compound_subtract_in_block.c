int main(void) {
    int a = 5;
    if (a > 4) {
        a -= 4;
        int a = 5;
        if (a > 4) {
            a -= 4;
        }
    }
    return a;
}