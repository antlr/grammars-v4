// test that we perform usual variable resolution/validation for switch
// statement bodies, including outside of case/default statements
int main(void) {
    int a = 1;
    switch (a) {
        // variable resolution must process this even though it's not reachable;
        // it still declares the variable/brings it into scope
        int b = 2;
        case 0:
            a = 3;
            int b = 2;  // error - duplicate declaration
    }
    return 0;
}