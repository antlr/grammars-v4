/* Test that we can call all the memory management functions */
#ifdef SUPPRESS_WARNINGS
#ifdef __clang__
#pragma clang diagnostic ignored "-Wincompatible-library-redeclaration"
#else
#pragma GCC diagnostic ignored "-Wbuiltin-declaration-mismatch"
#endif
#endif

void *malloc(unsigned long size);
void *realloc(void *ptr, unsigned long size);
void *calloc(unsigned long nmemb, unsigned long size);
void *aligned_alloc(unsigned long alignment, unsigned long size);
void free(void *ptr);

int main(void) {
    // allocate a buffer with malloc and populate it
    char *char_buffer = malloc(50);
    for (int i = 0; i < 50; i = i + 1) {
        char_buffer[i] = i;
    }

    // reallocate buffer
    char *char_buffer2 = realloc(char_buffer, 100);
    // we made it bigger, so update a value beyond the bounds of the old buffer
    // (whose value is undefined to start)
    char_buffer2[75] = 11;

    // make sure the contents from the original buffer are the same
    for (int i = 0; i < 50; i = i + 1) {
        if ( char_buffer2[i] != i) {
            return 1;
        }
    }

    // make sure the value beyond the bounds of the old buffer hast he value we assigned it
    if (char_buffer2[75] != 11) {
        return 2;
    }

    free(char_buffer2);

    // allocate a new buffer with calloc, make sure its contents are all zeros
    double *double_buffer = calloc(10, sizeof(double));
    for (int i = 0; i < 10; i = i + 1) {
        if (double_buffer[i]) {
            return 3;
        }
    }
    free(double_buffer);

    // allocate a buffer with aligned_alloc, make sure it has the correct alignment
    char_buffer = aligned_alloc(256, 256);
    // make sure it's 256 byte-aligned
    if ((unsigned long) char_buffer % 256) {
        return 4;
    }
    free(char_buffer);
    return 0;
}