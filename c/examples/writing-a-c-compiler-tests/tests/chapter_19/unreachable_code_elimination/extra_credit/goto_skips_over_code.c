/* Test that we eliminate code that goto jumps over */
int callee(void) {
    return 1;
}

int target(void) {
    int x = 10;
    goto end;
    x = callee(); // eliminate this
    end:
    return x;
}

int main(void) {
    return target();
}