/* test explicit casts between void * and other pointer types,
 * and between void * and integer types
 */

#ifdef SUPPRESS_WARNINGS
#ifdef __clang__
#pragma clang diagnostic ignored "-Wincompatible-library-redeclaration"
#else
#pragma GCC diagnostic ignored "-Wbuiltin-declaration-mismatch"
#endif
#endif

void *malloc(unsigned long size);
void free(void *ptr);
void *memcpy(void *s1, void *s2, unsigned long n);

int main(void) {
    // start with casts between void * and other types

    void *ptr = malloc(4 * sizeof(double));
    // cast void * to double *
    double *double_ptr = (double *)ptr;
    double_ptr[2] = 10.0;
    // cast double * back to void * - should round trip
    if ((void *)double_ptr != ptr) {
        return 1;
    }
    double result = double_ptr[2];

    if (result != 10.0) {
        // make sure assigning values through dbl_ptr worked
        return 2;
    }

    // now test cast from void * to integer
    unsigned long ul = (unsigned long)ptr;
    // address returned by malloc must have suitable alignment
    // for any basic data type, so it's divisible by 8
    if (ul % 8) {
        return 3;
    }

    free(ptr);

    // can also cast 0 to null pointer and back
    long zero = 0;
    ptr = (void *) zero;
    if (ptr) {
        return 4;
    }
    zero = (long) ptr;
    if (zero) {
        return 5;
    }
    return 0;
}
