/* Test that we can correcty handle subscript expressions that involve
 * a mix of pointers and arrays that decay to pointers
 */
int main(void) {
    int x = 0;
    int y = 1;
    int z = 2;

    // define two arrays of pointers
    int *arr[3] = { &x, &y, &z };
    int *arr2[3] = {&z, &y, &x};

    // an array of pointers to arrays of pointers
    int *(*array_of_pointers[3])[3] = {&arr, &arr2, &arr};
    if (array_of_pointers[0] != (int *(*)[3]) arr) {
        return 1;
    }

    if (array_of_pointers[1] != (int *(*)[3]) arr2) {
        return 2;
    }

    if (array_of_pointers[2] != (int *(*)[3]) arr) {
        return 3;
    }


    if (array_of_pointers[1][0][0] != &z) {
        return 4;
    }

    if (array_of_pointers[1][0][1] != &y) {
        return 5;
    }

    if (array_of_pointers[2][0][2][0] != 2) {
        return 6;
    }

    return 0;
}