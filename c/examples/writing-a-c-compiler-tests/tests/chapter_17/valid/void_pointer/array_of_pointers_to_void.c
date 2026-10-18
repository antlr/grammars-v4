/* Test using complex types derived from void
 * arrays of void are illegal, but arrays of pointer to void are allowed */

void *calloc(unsigned long nmemb, unsigned long size);
void free(void *ptr);

int main(void) {
    int i = 10;

    // declare an array of 4 pointers to void;
    // we can implicitly convert elements in this compound initializer to void
    void *arr[4] = {
        calloc(2, sizeof(int)),  // get a pointer to allocate memory
        &i,                      // implicitly convert int * to void *
        0,                       // convert null pointer constant to void *
        arr  // pointer to arr itself - implicitly convert (void *[4]) to void *
    };

    // first element points to 8 bytes, all initialized to 0
    // cast this to a long
    long *l = arr[0];
    if (*l) // l should point to value 0
        return 1;

    // second element points to i
    int elem_1_val = *(int *)arr[1];
    if (elem_1_val != 10)
        return 2;

    // 3rd element is a null pointer
    if (arr[2])
        return 3;

    // 4th element points to arr itself! trippy!
    if (arr[3] != arr)
        return 4;
    free(arr[0]);  // free allocated memory pointed to by first element
    return 0;
}