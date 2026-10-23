/* Test that in the expression &*e, we just evaluate e and ignore the & and *.
 * This is why &*null_ptr is valid, even though dereferencing a null pointer
 * would normally produce a runtime error
 */
int main(void) {
    int *null_ptr = 0;
    if (&*null_ptr != 0) // &*null_ptr is equivalent to null_ptr
        return 1;

    // do the same with multiple levels of indirection
    int **ptr_to_null = &null_ptr;

    // &**ptr_to_null is equivalent to *ptr_to_null,
    // which evaluates to the value of null_ptr
    if (&**ptr_to_null)
        return 2;

    return 0;
}