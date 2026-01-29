/* Basic test of union type declarations, initializers, and member access */

// declare a union type
union u {
    double d;
    long l;
    unsigned long ul;
    char c;
};

int main(void) {
    // declare and initialize a union
    union u x = {20}; // this initializes first member

    // read member
    if (x.d != 20.0) {
        return 1; // fail
    }

    // assign/read through pointer
    union u *ptr = &x;
    ptr->l = -1l;
    if (ptr->l != -1l) {
        return 2; // fail
    }

    // read through other members
    if (ptr->ul != 18446744073709551615UL) {
        return 3; // fail
    }

    if (x.c != -1) {
        return 4; // fail
    }
    return 0;
}