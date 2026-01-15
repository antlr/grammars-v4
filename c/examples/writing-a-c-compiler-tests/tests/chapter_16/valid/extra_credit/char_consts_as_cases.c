// Test that we can use character constants as cases in switch statements
int main(void) {
    static int i = 65;
    switch (i) {
        case 100l:
            return 1;  // fail
        case 'A':
            return 0;  // success
        case 'B':
            return 2;  // fail
        case 2000u:
            return 3;  // fail
        default:
            return -1;  // fail
    }
}