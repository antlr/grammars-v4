// Pointers to a union object and to its members all compare equal
struct s {
    int i;
};

union u {
    char arr[3];
    double d;
    struct s my_struct;
};

union u my_union; // don't initialize, so it will be initialized to zero

int main(void) {
    union u* u_ptr = &my_union;

    // compare pointer to whole union w/ pointers to members,
    // using both == and !=
    if ((void*)u_ptr != (void*)&(u_ptr->arr)) {
        return 1; // fail
    }

    if (!((void*)u_ptr == (void*)&(u_ptr->d))) {
        return 2; // fail
    }

    if ((void*)&(u_ptr->my_struct) != u_ptr) {
        return 3; // fail
    }

    // compare member pointers
    if (my_union.arr != (char*)&my_union.d) {
        return 4; // fail
    }

    if (!(&my_union.arr[0] >= (char *) &my_union.my_struct.i)) {
        return 5; // fail
    }

    if (! ((char *) (&u_ptr->d) <= (char *) &u_ptr->my_struct)) {
        return 6; // fail
    }

    return 0;
}