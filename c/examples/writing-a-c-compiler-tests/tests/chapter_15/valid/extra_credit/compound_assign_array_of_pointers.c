// Compound assignment where lval is a subscript expression with pointer type
int main(void) {
    // array of 3 pointers to arrays of 4 ints
    static int (*array_of_pointers[3])[4] = {0, 0, 0};
    int array1[4] = {100, 101, 102, 103};
    int nested_array[2][4] = {
        {200, 201, 202, 203},
        {300, 301, 302, 303}
    };
    array_of_pointers[0] = &array1;
    array_of_pointers[1] = &nested_array[0];
    array_of_pointers[2] = &nested_array[1];

    array_of_pointers[0] += 1; // points one past the end of array1
    if (array_of_pointers[0][-1][3] != 103) {
        return 1; // fail
    }

    // swap these so they point to last and first elements of nested_array, respectively
    array_of_pointers[1] += 1;
    array_of_pointers[2] -= 1;
    if (array_of_pointers[1][0][3] != 303) {
        return 2; // fail
    }
    if (array_of_pointers[2][0][3] != 203) {
        return 3; // fail
    }

    return 0;
}