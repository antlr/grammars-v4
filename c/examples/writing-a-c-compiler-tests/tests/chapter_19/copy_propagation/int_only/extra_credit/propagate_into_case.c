int globvar = 0;

int callee(int arg) {
    globvar = arg;
    return 0;
}

int target(int flag) {
    int arg = 10;
    switch (flag) {
        case 1:
            arg = 20;
            break;
        case 2:
            // replace arg w/ 10 here - previous assignment
            // doesn't kill this b/c we never pass through it to get here
            callee(arg);
            break;
        default:
            globvar = -1;
    }
    return 0;
}

int main(void) {
    target(2);
    if (globvar == 10) {
        return 0; // success
    }

    return 1; // fail
}