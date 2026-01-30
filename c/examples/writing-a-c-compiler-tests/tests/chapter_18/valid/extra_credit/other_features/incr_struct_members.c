// Test prefix and postfix ++ and -- with structure members

struct inner {
    char c;
    unsigned int u;
};

struct outer {
    unsigned long l;
    struct inner *in_ptr;
    int array[3];
};

void *calloc(unsigned long nmemb, unsigned long size);

int main(void) {
    struct outer my_struct = {
        // l
        9223372036854775900ul,
        // in_ptr
        calloc(3, sizeof (struct inner)),
        // array
        {-1000, -2000, -3000},
    };
    struct outer *my_struct_ptr = &my_struct;

    // prefix ++
    if (++my_struct.l != 9223372036854775901ul) {
        return 1; // fail
    }

    // prefix --
    if (--my_struct.in_ptr[0].u != 4294967295U) { // unsigned wraparound
        return 2; // fail
    }

    // postfix ++
    if (my_struct.in_ptr->c++) {
        return 3; // fail
    }

    // postfix --
    if (my_struct_ptr->array[1]-- != -2000) {
        return 4; // fail
    }

    // validate current state of my_struct - make sure we performed updates
    // and didn't clobber anything
    if (my_struct_ptr->l != 9223372036854775901ul) {
        return 5; // fail
    }

    if (my_struct.in_ptr->c != 1) {
        return 6; // fail
    }
    if (my_struct_ptr->in_ptr->u !=  4294967295U) {
        return 7; // fail
    }

    if (my_struct_ptr->array[1] != -2001) {
        return 8; // fail
    }

    if (my_struct_ptr->array[0] != -1000 || my_struct_ptr->array[2] != -3000) {
        return 9; // fail
    }

    // ++/-- w/ pointers to structs
    // first let's populate the struct array at my_struct_ptr->in_ptr
    my_struct_ptr->in_ptr[1].c = -1;
    my_struct_ptr->in_ptr[1].u = 1u;
    my_struct_ptr->in_ptr[2].c = 'X';
    my_struct_ptr->in_ptr[2].u = 100000u;

    (++my_struct_ptr->in_ptr)->c--; // decrement struct array[1].c
    my_struct_ptr->in_ptr++->u++; // decrement stuct_array[1].u, increment in_ptr

    // validate - in_ptr currently points to array member at index 2

    // element 0 (now at index -2) should have same values as last time we checked
    if (my_struct_ptr->in_ptr[-2].c != 1 || my_struct_ptr->in_ptr[-2].u != 4294967295U) {
        return 10;
    }

    // we decremented c in element 1 (now at index -1), didn't change u
    if (my_struct_ptr->in_ptr[-1].c != -2) {
        return 11; // fail
    }

    if (my_struct_ptr->in_ptr[-1].u != 2) {
        return 12; // fail
    }

    // didn't change any values in last array element (now at index 0)
    if (my_struct_ptr->in_ptr[0].c != 'X' || my_struct_ptr->in_ptr[0].u != 100000u) {
        return 13; // fail
    }

    return 0;
}