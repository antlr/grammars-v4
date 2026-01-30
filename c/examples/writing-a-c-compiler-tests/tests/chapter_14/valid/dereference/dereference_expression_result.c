/* Make sure we can dereference any expression of pointer type,
 * not just variables */

int *return_pointer(void) {
    static int var = 10;
    return &var;
}

int one = 1;

int main(void) {
    int val = 100;
    int *ptr_var = &val;

    // First try reading pointers that result from function call
    if (*return_pointer() != 10) {
        return 1;
    }

    // Now dereference pointers from ternary expressions
    if (*(one ? return_pointer() : ptr_var) != 10)
        return 2;

    if (*(one - 1 ? return_pointer() : ptr_var) != 100) {
        return 3;
    }


    // now dereference result of assignment expression
    int *ptr_to_one = &one;
    if (*(ptr_var = ptr_to_one) != 1) {
        return 4;
    }

    // Now try to update values through these pointers
    *return_pointer() = 20;
    *(one ? ptr_var : return_pointer()) = 30; // this updates the static variable one

    // Validate that the values of the pointed-to objects were updated
    if (*return_pointer() != 20) {
        return 5;
    }
    if (*ptr_var != 30) {
        return 6;
    }
    if (one != 30) {
        return 7;
    }

    return 0;
}