/* Make sure we can implicity convert null pointer constants to pointer type */

// convert static variable initializers
double *d = 0l;
int *i = 0ul;
int *i2 = 0u;

int expect_null_param(int *val)
{
    // validate that this is a null pointer
    return (val == 0ul);
}

long *return_null_ptr(void)
{
    return 0; // convert return value to pointer
}

int main(void)
{
    int x = 10;
    int *ptr = &x;

    // check static initializers
    if (d) {
        return 1;
    }

    if (i) {
        return 2;
    }
    if (i2) {
        return 3;
    }

    // convert to pointer for assignment
    ptr = 0ul;
    if (ptr) {
        return 4;
    }

    // convert pointer in non-static initializer
    int *y = 0;
    if (y != 0)
        return 5;

    // convert function argument to pointer
    if (!expect_null_param(0)) {
        return 6;
    }

    // return_null_ptr converts a null pointer constant to a pointer
    long *null_ptr = return_null_ptr();
    if (null_ptr != 0) {
        return 7;
    }

    // convert ternary operand to null pointer
    ptr = &x; // now pointer is non-null
    int *ternary_result = 10 ? 0 : ptr;
    if (ternary_result) {
        return 8;
    }

    return 0;
}