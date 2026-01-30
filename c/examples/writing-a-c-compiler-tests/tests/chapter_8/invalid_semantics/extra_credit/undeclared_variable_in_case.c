// test that we perform usual variable resolution/validation within case
// statements
int main(void) {
    int a = 10;
    switch (a) {
        case 1:
            return b;
            break;

        default:
            break;
    }
    return 0;
}