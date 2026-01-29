// given a nested array of longs, return a pointer to one row in the array
long (*return_row(long (*arr)[3][4], int idx))[4] {
    return arr[idx];
}