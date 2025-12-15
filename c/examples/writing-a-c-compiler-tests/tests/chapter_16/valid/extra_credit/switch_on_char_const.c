// Test that we can use character constant in switch statement
int main(void) {
    switch ('x') {
        case 1:
            return 1;  // fail
        case 2:
            return 2;  // fail
        case 120:
            return 0;  // success
        default:
            return -1;  // fail
    }
}