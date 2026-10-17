/* Make sure we can take the address of function arguments,
 * not just variables */

int addr_of_arg(int a) {
    int *ptr = &a;
    *ptr = 10;
    return a;
}

int main(void) {
    // the parameter a is an lvalue with an address,
    // but the corresponding argument doesn't have to be
    int result = addr_of_arg(-20);
    if (result != 10) {
        return 1;
    }

    // try again with an lvalue; make sure original value doesn't change
    int var = 100;
    result = addr_of_arg(var);
    if (result != 10) {
        return 2;
    }
    if (var != 100) {
        return 3;
    }
    return 0;
}