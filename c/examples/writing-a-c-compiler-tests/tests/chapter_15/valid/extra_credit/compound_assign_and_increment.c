// Combination of compound assignment and increment/decrement with subscript expressions
int main(void) {
    int arr[4] = {-1, -2, -3, -4};
    int *ptr = arr;
    int idx = 2;

    // arr[2] *= -3;
    // after expression, ptr points to arr[1] and idx is 3
    if ((ptr++[idx++] *= 3) != -9) {
        return 1; // fail
    }
    if (*ptr != -2) {
        return 2; // fail
    }
    if (idx != 3) {
        return 3; // fail
    }
    idx--;
    // arr[3] += 4 results in 4
    if ((--ptr)[3] += 4) {
        return 4; // fail
    }

    // validate all array elements
    if (arr[0] != -1 || arr[1] != -2 || arr[2] != -9 || arr[3] != 0) {
        return 5; // fail
    }
    return 0; // success
}