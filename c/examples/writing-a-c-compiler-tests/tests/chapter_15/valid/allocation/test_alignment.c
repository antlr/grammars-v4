/* Test that we allocate enough space for arrays on the stack
 * and that they're correctly aligned
 * all three arrays are >= 16 bytes so they must be 16-byte aligned
 */

int check_alignment(int *ptr) {
    unsigned long addr = (unsigned long) ptr;
    return (addr % 16 == 0);
}

int main(void)
{
    // this initializes each element in each array to zero
    int arr[5] = {0};
    int arr2[7] = {0};
    int arr3[2][2] = {{0}};

    // check alignment of arr
    if (!check_alignment(arr)) {
        return 1;
    }

    // assign values to arr
    for (int i = 0; i < 5; i = i + 1)
        arr[i] = i;

    // check alignment of arr2
    if (!check_alignment(arr2)) {
        return 2;
    }

    // make sure we didn't overwrite arr2
    for (int i = 0; i < 7; i = i + 1)
        if (arr2[i])
            return 3;

    // now update arr2
    for (int i = 0; i < 7; i = i + 1){
        arr2[i] = -i;
    }

    // check alignment of arr3
    if (!check_alignment((int *)arr3)) {
        return 4;
    }

    // check values of arr1 and arr3;
    // make sure we didn't clobber them when overwriting arr2
    for (int i = 0; i < 5; i = i + 1) {
        if (arr[i] != i) {
            return 5;
        }
    }

    for (int i = 0; i < 2; i = i + 1)
        for (int j = 0; j < 2; j = j + 1)
            if (arr3[i][j] != 0)
                return 6;

    return 0;
}