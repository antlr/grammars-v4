// Test ++ and -- operations on global variables;
int i = 0;
int j = 0;

int incr_i(void){
    // expect i = 1
    if (i == 1) {
        i++;
        ++i;
    }
    return 0;
}

int decr_j(void) {
    // expect j = -1
    if (j == -1) {
        j--;
    }
    return 0;
}

int main(void) {
    // should take second branch; result of i++ is value before incrementing (i.e. 0)
    // but we evaluate the branch after the side effect of incrementing the value
    i++ ? 0 : incr_i();

    // after fun call, expect i = 3
    if (i != 3) {
        // fail
        return 1;
    }

    // should take first branch; result of --j is value after decrementing
    --j? decr_j(): 0;

    // after fun call, expect j = -2
    if (j != -2) {
        // fail
        return 2;
    }

    return 0; // success
}