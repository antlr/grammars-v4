/* Test finding the common type of void * and other pointer types (it's always
 * void *) */

void *calloc(unsigned long nmemb, unsigned long size);
void free(void *ptr);

int main(void) {
    // get a pointer to void
    void *void_ptr = calloc(3, sizeof(unsigned int));

    // we'll use 'array' a a pointer to a complete object
    unsigned int array[3] = {1, 2, 3};

    // like other pointers, void * can be compared to null pointer constant
    if (void_ptr == 0)
        return 1;

    // compare with ==
    if (void_ptr == array)
        return 2;

    // compare with !=
    if (!(void_ptr != array))
        return 3;

    // use in conditional
    // note that result of conditional is void * so it can be implicitly
    // converted to any pointer type

    // also use a void * as the condition here just for fun
    static void *null_ptr = 0;
    int *my_array = null_ptr ? void_ptr : array;

    // note: effective type of this object is unsigned int,
    // so we're allowed to access it with an expression of
    // the corresponding signed type, int.
    // if this object were void_ptr instead of array, this would be undefined
    int array_element = my_array[1];

    if (array_element != 2) {
        return 4;
    }

    free(void_ptr);
    return 0;
}