// Reject duplicate cases where one uses an integer constant and one uses
// a character constant
int main(void) {
    static int i = 120;
    switch (i) {
        case 'x':  // ASCII value 120
            return 1;
        case 120:  // duplicate
            return 2;
        default:
            return 3;
    }
}