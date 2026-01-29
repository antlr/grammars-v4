// Test using -> to copy entire structures,
// including large structures w/ members of different sizes

void *calloc(unsigned long nmemb, unsigned long size);
void *malloc(unsigned long size);

struct inner {
    double d;
    int i;
};

struct outer {
    char a;
    char b;
    struct inner substruct;
};

struct outermost {
    int i;
    struct outer *nested_ptr;
    struct outer nested_struct;
};

// case 1: x = y->z
int test_copy_from_member_pointer(void) {
    struct inner small = {0.0, 0};
    struct outer *outer_ptr = malloc(sizeof(struct outer));
    outer_ptr->a = 100;
    outer_ptr->substruct.d = 21.5;
    outer_ptr->substruct.i = 100001;

    small = outer_ptr->substruct;

    if (small.d != 21.5 || small.i != 100001) {
        return 0;
    }

    return 1;  // success
}

// case 2: y->z = x
int test_copy_to_member_pointer(void) {
    struct inner small = {99.25, 987654};
    struct outer *outer_ptr = calloc(1, sizeof(struct outer));
    outer_ptr->substruct = small;

    // validate
    if (outer_ptr->substruct.d != 99.25 || outer_ptr->substruct.i != 987654) {
        return 0;
    }

    // make sure we didn't clobber other members in outer_ptr
    if (outer_ptr->a || outer_ptr->b) {
        return 0;
    }

    return 1;  // success
}

// case 3: a = x->y->z
int test_copy_from_nested_member_pointer(void) {
    struct inner small = {99.25, 987654};
    struct outermost *outer_ptr = calloc(1, sizeof(struct outermost));
    outer_ptr->nested_ptr = calloc(1, sizeof(struct outer));

    // initialize allocated pointer
    outer_ptr->i = -5;
    outer_ptr->nested_ptr->a = 101;
    outer_ptr->nested_ptr->b = 102;
    outer_ptr->nested_ptr->substruct.d = 77.5;
    outer_ptr->nested_ptr->substruct.i = 88;

    small = outer_ptr->nested_ptr->substruct;

    // validate small
    if (small.d != 77.5 || small.i != 88) {
        return 0;
    }

    // make sure we didn't overwrite any bytes of outer_ptr
    if (outer_ptr->i != -5 || outer_ptr->nested_struct.a) {
        return 0;
    }

    return 1;  // success
}

// case 4: x->y->z = a
int test_copy_to_nested_member_pointer(void) {
    struct inner small = {99.25, 987654};
    struct outermost *outer_ptr = calloc(1, sizeof(struct outermost));
    outer_ptr->nested_ptr = calloc(1, sizeof(struct outer));

    outer_ptr->nested_ptr->substruct = small;

    // validate outer_ptr->nested_ptr->substrct
    if (outer_ptr->nested_ptr->substruct.d != 99.25 ||
        outer_ptr->nested_ptr->substruct.i != 987654) {
        return 0;
    }

    // make sure we didn't overwrite neighboring members of nested_ptr
    if (outer_ptr->nested_ptr->a || outer_ptr->nested_ptr->b) {
        return 0;
    }

    return 1;  // success
}

// case 5: assign one member to another,
// copy to/from x->y.z and x.y->z
int test_mixed_nested_access(void) {
    struct outermost s1 = {100, 0, {0, 0, {0, 0}}};
    struct outermost *s2_ptr = calloc(1, sizeof(struct outermost));

    // populate s1
    s1.i = 2147483647;
    s1.nested_ptr = calloc(1, sizeof(struct outermost));
    s1.nested_ptr->a = 125;
    s1.nested_ptr->b = 126;
    s1.nested_ptr->substruct.d = -50.;
    s1.nested_ptr->substruct.i = -70;
    s1.nested_struct.a = 101;
    s1.nested_struct.b = 102;

    // populate s2_ptr
    s2_ptr->i = -2147483647;
    s2_ptr->nested_ptr = calloc(1, sizeof(struct outermost));
    s2_ptr->nested_ptr->a = 5;
    s2_ptr->nested_ptr->b = 6;
    s2_ptr->nested_struct.substruct.d = 8.e8;
    s2_ptr->nested_struct.substruct.i = -5;

    // nested copy
    s1.nested_ptr->substruct = s2_ptr->nested_struct.substruct;

    // validate
    if (s1.nested_ptr->substruct.d != 8.e8 ||
        s1.nested_ptr->substruct.i != -5) {
        return 0;
    }

    // make sure we didn't clobber neighboring member in s1.nested_ptr
    if (s1.nested_ptr->a != 125 || s1.nested_ptr->b != 126) {
        return 0;
    }

    return 1;  // success
}

// case 6: assign to member of struct pointer produced by cast expression,
// ((struct s *)x) -> y = z
int test_member_from_cast(void) {
    struct inner small = {20.0, 10};

    void *outer_ptr = calloc(1, sizeof(struct outer));
    ((struct outer *)outer_ptr)->substruct = small;

    // validate
    if (((struct outer *)outer_ptr)->substruct.d != 20.0 ||
        ((struct outer *)outer_ptr)->substruct.i != 10) {
        return 0;
    }

    return 1;  // success
}

int main(void) {
    if (!test_copy_from_member_pointer()) {
        return 1;
    }

    if (!test_copy_to_member_pointer()) {
        return 2;
    }

    if (!test_copy_from_nested_member_pointer()) {
        return 3;
    }

    if (!test_copy_to_nested_member_pointer()) {
        return 4;
    }

    if (!test_mixed_nested_access()) {
        return 5;
    }

    if (!test_member_from_cast()) {
        return 6;
    }

    return 0;
}