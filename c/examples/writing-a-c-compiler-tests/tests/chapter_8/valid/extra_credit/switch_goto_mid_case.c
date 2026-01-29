int main(void) {
    int a = 0;
    // a goto statement can jump to any point in a switch statement, including the middle of a case
    goto mid_case;
    switch (4) {
        case 4:
            a = 5;
        mid_case:
            a = a + 1;
            return a;
    }
    return 100;
}