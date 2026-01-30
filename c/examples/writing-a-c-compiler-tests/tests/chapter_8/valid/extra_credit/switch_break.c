// a basic test that the correct case in a switch statement
// is executed and that we can break out of a switch statement

int main(void) {
    int a = 5;
    switch (a) {
        case 5:
            a = 10;
            break;
        case 6:
            a = 0;
            break;
    }
    return a;
}