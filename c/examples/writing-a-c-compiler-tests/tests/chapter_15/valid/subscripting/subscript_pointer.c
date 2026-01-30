/* Test that we can apply subscript expressions to all pointers,
 * not just pointers that decayed from arrays */


int subscript_pointer_to_pointer(int **x) {
    return x[0][0];
}

int main(void) {
    int a = 3;
    int *ptr = &a;

    // subscript a pointer
    if (ptr[0] != 3) {
        return 1;
    }

    // subscript a pointer to a pointer
    int **ptr_ptr = &ptr;
    if (ptr_ptr[0][0] != 3) {
        return 2;
    }

    // pass pointer to pointer as a function argument, which will be subscripted
    // note that this NOT equivalent to pointer to array!
    int dereferenced = subscript_pointer_to_pointer(ptr_ptr);
    if (dereferenced != 3) {
        return 3;
    }
    return 0;
}