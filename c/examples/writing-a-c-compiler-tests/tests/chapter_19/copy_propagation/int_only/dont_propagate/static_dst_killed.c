/* Function calls kill copies to variables with static storage duration */
int x;

int update_x(void) {
    x = 4;
    return 0;
}

int target(void) {
    x = 3;       // generate x = 3
    update_x();  // kill x = 3
    return x;    // can't propagte b/c it's static
}

int main(void) {
    // validate return value and value of x after function call
    if (target() != 4) {
        return 1;
    }

    if (x != 4) {
        return 2;
    }

    return 0;  // success
}