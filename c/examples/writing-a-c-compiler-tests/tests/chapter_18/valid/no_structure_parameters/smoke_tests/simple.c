/* basic test of struct type declarations, compound initializers, and member
 * access */

// declare a structure type
struct pair {
    int a;
    int b;
};

int main(void) {
    // declare and initialize a structure
    struct pair x = {1, 2};

    // read structure members with . operator
    if (x.a != 1 || x.b != 2) {
        return 1;
    }

    // read structure members with -> operator
    struct pair *x_ptr = &x;
    if (x_ptr->a != 1 || x_ptr->b != 2) {
        return 2;
    }

    return 0;
}