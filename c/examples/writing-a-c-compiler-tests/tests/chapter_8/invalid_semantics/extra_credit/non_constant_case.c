int main(void) {
    int a = 3;
    switch(a + 1) {
        case 0: return 0;
        case a: return 1; // case statement values must be constant
        case 1: return 2;
    }
}