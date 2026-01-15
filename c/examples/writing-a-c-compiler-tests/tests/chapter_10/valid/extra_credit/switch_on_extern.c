int update_x(void);

// test that we can use an external variable in a switch statement
int main(void) {
    update_x(); // set x to 4
    extern int x; // bring x into scope
    switch(x) {
        case 0: return 1; // fail
        case 1: return 2; // fail
        case 4: return 0; // success!
        default: return 4; // fail

    }
}

int x;

int update_x(void) {
    x = 4;
    return 0;
}
