/* Test comparisons between pointers to structures and structure members:
 * 1. Pointers to a structure and its first member compare equal
 * 2. Pointers to later structure members compare greater than pointers
 *    to earlier structure members
 */

struct three_ints {
    int a;
    int b;
    int c;
};

void* calloc(unsigned long nmem, unsigned long size);

int main(void) {
    struct three_ints* my_struct = calloc(1, sizeof(struct three_ints));

    // compare struct pointer with pointer to first member
    if ((void*)my_struct != &my_struct->a) {
        return 1; // fail
    }

    // do the same with a relational operator
    if (!((int *)my_struct <= &my_struct->a)) {
        return 2; // fail
    }

    // compare earlier to later members using a few different relational operators
    if (&my_struct->c <= &my_struct->a) {
        return 3;  // fail
    }

    if (&my_struct->b > &my_struct->c) {
        return 4;  // fail
    }

    if (!(&my_struct->b > &my_struct->a)) {
        return 5;  // fail
    }

    return 0; // success
}