int main(void) {
    int a;
    int b = a = 7;
    switch (a + b) {
        default: return 0;
        case 2: return 1;
    }

}