/* Test that we recognize aliased non-static variables are live
 * just after function calls but dead at function exit
 * */

int b = 0;

void callee(int *ptr) {
    b = *ptr;
    *ptr = 100;
}

int target(void) {
    int x = 10;
    callee(&x);  // generates all aliased variables (i.e. x)
    int y = x;
    x = 50;  // this is dead
    return y;
}

int main(void) {
    int a = target();
    if (a != 100) {
        return 1; // fail
    }
    if (b != 10) {
        return 2; // fail
    }
    return 0; // success
}