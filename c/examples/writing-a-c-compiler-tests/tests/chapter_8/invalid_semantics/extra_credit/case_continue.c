int main(void) {
    int a = 3;
    switch(a + 1) {
        case 0:
            // continue can only break out of loops, not switch statements
            continue;
        default: a = 1;
    }
    return a;
}