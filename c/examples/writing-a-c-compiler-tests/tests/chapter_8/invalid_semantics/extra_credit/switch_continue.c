int main(void) {
    int a = 3;
    switch(a + 1) {
        case 0:
            a = 4;
            // continue not permitted in switch statements
            // (unless the switch is inside a loop)
            continue;
        default: a = 1;
    }
    return a;
}