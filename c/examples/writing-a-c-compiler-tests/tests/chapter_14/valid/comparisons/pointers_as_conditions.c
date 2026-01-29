/* Test out using pointers in boolean expressions or as controlling conditions,
 * which implicitly compares them to zero
 */

#ifdef SUPPRESS_WARNINGS
#ifdef __clang__
#pragma clang diagnostic ignored "-Wliteral-conversion"
#endif
#endif


long *get_null_pointer(void) {
    return 0;
}

int main(void)
{
    long x;
    long *ptr = &x;
    long *null_ptr = get_null_pointer();

    // note that pointers can appear in boolean expressions
    // with operands of any other type
    if (5.0 && null_ptr) {
        return 1;
    }

    int a = 0;
    if (!(ptr || (a = 10))) {
        return 2;
    }

    // make sure the || expression short-circuited
    if (a != 0) {
        return 3;
    }

    // apply ! to pointer
    if (!ptr) {
        return 4;
    }

    // use a pointer in a ternary expression
    int j = ptr ? 1 : 2;
    int k = null_ptr ? 3 : 4;
    if (j != 1) {
        return 5;
    }

    if (k != 4) {
        return 6;
    }

    // use a pointer as the controlling condition in a loop
    int i = 0;
    while (ptr)
    {
        if (i >= 10) {
            ptr = 0;
            continue;
        }
        i = i + 1;
    }
    if (i != 10) {
        return 7;
    }

    return 0;
}