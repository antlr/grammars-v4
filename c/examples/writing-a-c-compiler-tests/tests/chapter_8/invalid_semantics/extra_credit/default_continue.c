int main(void) {
    int a = 3;
    switch(a + 1) {
        case 0:
            a = 1;
        // make sure the pass that labels loops and checks for invalid
        // break/continue statements traverses default statements
        default: continue;
    }
    return a;
}