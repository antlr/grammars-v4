/* Test that updating a value through a pointer does not kill that pointer */

void exit(int status);  // from standard library

void check_pointers(int a, int b, int *ptr1, int *ptr2) {
    if (a != 100 || b != 101) {
        exit(1);
    }

    if (*ptr1 != 60 || *ptr2 != 61) {
        exit(2);
    }
    return;
}

int callee(int *p1, int *p2) {
    if (p1 != p2) {
        exit(3);
    }
    if (*p2 != 10) {
        exit(4);
    }
    return 0;  // success
}

int target(int *ptr, int *ptr2) {
    // first, call another function, with these arguments
    // in different positions than in target or callee, so we can't
    // coalesce them with the param-passing registers or each other
    check_pointers(100, 101, ptr, ptr2);

    ptr2 = ptr;  // generate copy
    *ptr = 10;   // Store(10, ptr) does NOT kill copy

    // both arguments to callee should be the same
    return callee(ptr, ptr2);
}

int main(void) {
    int x = 60;
    int y = 61;
    return target(&x, &y);
}