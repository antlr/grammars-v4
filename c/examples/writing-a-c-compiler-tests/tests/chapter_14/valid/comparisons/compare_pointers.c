/* Test comparing pointers with == and != */

int main(void) {
    int a = 0;
    int b;

    int *a_ptr = &a;
    int *a_ptr2 = &a;
    int *b_ptr = &b;

    if (a_ptr == b_ptr) {
        return 1;
    }

    if (a_ptr != a_ptr2) {
        return 2;
    }

    if (!(a_ptr == a_ptr2)) {
        return 3;
    }

    if (!(a_ptr != b_ptr)) {
        return 4;
    }

    // if you assign dereferenced value of one pointer to another, the pointers
    // themselves are still not equal
    *b_ptr = *a_ptr;
    if (a_ptr == b_ptr) {
        return 5;
    }
    // if you assign one pointer to another, they will be equal afterwards,
    // just like any other variable
    b_ptr = a_ptr;
    if (b_ptr != a_ptr) {
        return 6;
    }

    return 0;
}