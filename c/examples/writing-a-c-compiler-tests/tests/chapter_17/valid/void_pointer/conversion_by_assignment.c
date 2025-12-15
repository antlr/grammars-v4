/* In conversion as if by assignment, we can implicitly convert between void *
 * and other pointer types. */

#ifdef SUPPRESS_WARNINGS
#ifdef __clang__
#pragma clang diagnostic ignored "-Wincompatible-library-redeclaration"
#else
#pragma GCC diagnostic ignored "-Wbuiltin-declaration-mismatch"
#endif
#endif

void *malloc(unsigned long size);
void free(void *ptr);
int memcmp(void *s1, void *s2, unsigned long n);

void *return_ptr(char *i) {
    // get pointer to i[3], implicitly cast from char * to void *
    return i + 3;
}

int check_char_ptr_argument(char *pointer, char expected_val) {
    // note: strict aliasing rules let us inspect any object through char,
    // so this is well-defined even if pointer points to an object
    // whose effective type is not char
    return *pointer == expected_val;
}

int *return_void_ptr_as_int_ptr(void *pointer) {
    return pointer; // implicitly convert pointer type from void * to int *
}

// allocate an array of n doubles
double *get_dbl_array(unsigned long n) {
    return (double *) malloc(n * sizeof (double));
}

// populate an array of n doubles with value d
void set_doubles(double *array, unsigned long n, double d) {
    for (unsigned long i = 0; i < n; i = i + 1) {
        array[i] = d;
    }
    return;
}


void *return_dbl_ptr_as_void_ptr(double *ptr) {
    return ptr; // implicitly convert pointer type from double * to void *
}

int main(void) {
    void *four_bytes = malloc(4);

    /* First look at conversions from void */

    /* Initializer: implicitly convert void * to int * */
    int *int_ptr = four_bytes;
    *int_ptr = -1; // set all bytes to 1

    /* Function argument: convert four_bytes to function argument
     * with type (char *). If we interpret lowest byte of four_bytes
     * as a char its value should still be -1
     */
    if (!check_char_ptr_argument(four_bytes, -1)) {
        return 1;
    }

    /* Return value: if we call a function that implicitly converts four_bytes
     * to an int *, the result should equal int_ptr
     */
    if (return_void_ptr_as_int_ptr(four_bytes) != int_ptr) {
        return 2;
    }

    /* Assignment: we can implicitly convert void *
     * to other pointer types by assignment
     * and have it round trip */
    double *dbl_ptr = four_bytes;
    int (*complicated_ptr)[3][2][5] = four_bytes;
    long *long_ptr = four_bytes;
    if (dbl_ptr != four_bytes || complicated_ptr != four_bytes || long_ptr != four_bytes) {
        return 3;
    }

    free(four_bytes);

    /* Now look at conversions _to_ void */

    double *dbl_array = get_dbl_array(5);

    /* Initializer - convert double * to void * */
    void *void_array = dbl_array;

    /* Function argument - convert void_array to parameter with type double * */
    set_doubles(void_array, 5, 4.0);
    // make sure that worked
    if (dbl_array[3] != 4.0) {
        return 4;
    }

    /* Return value: if we call a function to convert dbl_array to void *,
     * result should equal void_array */
    if (return_dbl_ptr_as_void_ptr(dbl_array) != void_array) {
        return 5;
    }

    /* Assignment: assign some other pointer types to a void * object */
    void *some_other_ptr = 0;

    some_other_ptr = dbl_array; // convert double * to void *
    if (some_other_ptr != void_array) {
        return 6;
    }

    some_other_ptr = &some_other_ptr; // convert void ** to void *
    if (some_other_ptr == void_array) {
        return 7;
    }

    complicated_ptr = 0;
    some_other_ptr = complicated_ptr;
    if (some_other_ptr) { // null pointer is always 0 no matter what type it points to
        return 8;
    }

    free(dbl_array);

    /* Convert void * to another pointer type in a compound initializer */
    long *long_ptr_array[3] = {
        // convert three pointers from (void *) to (long *)
        malloc(sizeof(long)), malloc(sizeof(long)), malloc(sizeof(long))
    };

    //make sure we can read/write this malloc'ed memory
    *long_ptr_array[0] = 100l;
    *long_ptr_array[1] = 200l;
    *long_ptr_array[2] = 300l;
    long sum = (*long_ptr_array[0] + *long_ptr_array[1] + *long_ptr_array[2]);
    if (sum != 600l) {
        return 9;
    }
    free(long_ptr_array[0]);
    free(long_ptr_array[1]);
    free(long_ptr_array[2]);

    /* one last test case: implicitly convert pointers to void * when we pass them to standard library calls */

    long arr1[3] = {1, 2, 3};
    long arr2[3] = {1, 2, 3};
    long arr3[3] = {1, 2, 4};
    if (memcmp(arr1, arr2, sizeof arr1) != 0) { // these compare equal
        return 10;
    }
    if (memcmp(arr1, arr3, sizeof arr2) != -1) { //arr1 should compare less than arr3, since 3 < 4
        return 11;
    }
    return 0;
}
