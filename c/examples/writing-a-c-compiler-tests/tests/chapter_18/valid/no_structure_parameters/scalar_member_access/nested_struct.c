/* Test accessing nested structures members, through dot, arrow, and subscript
 * operators */

void *calloc(unsigned long nmemb, unsigned long size);
void *malloc(unsigned long size);

struct inner {
    double a;
    char b;
    int *ptr;
};

struct outer {
    unsigned long l;
    struct inner *in_ptr;
    struct inner in_array[4];
    int bar;
    struct inner in;
};

int ptr_target;  // static int for 'ptr' member in various struct inners to
                 // point to

int test_auto_dot(void) {
    // Test nested access in struct with automatic storage duration,
    // using only . operator
    struct outer s;

    // write through nested accesses
    s.in.a = 1.0;
    s.in.b = 2;
    s.in.ptr = &ptr_target;

    // read through nested accesses
    if (s.in.a != 1.0 || s.in.b != 2 || s.in.ptr != &ptr_target) {
        return 0;
    }

    // get address of nested member
    char *char_ptr = &s.in.b;
    if (*char_ptr != 2) {
        return 0;
    }

    // dereference nested member
    *s.in.ptr = 5;
    if (ptr_target != 5) {
        return 0;
    }

    // copy one member to another
    s.in.a = s.in.b;
    if (s.in.a != 2.0) {
        return 0;
    }

    return 1;  // success
}

int test_static_dot(void) {
    // Test nested access in struct with static storage duration,
    // using only . operator
    static struct outer s;

    // write through nested accesses
    s.in.a = 1.0;
    s.in.b = 2;
    s.in.ptr = &ptr_target;

    // read through nested accesses
    if (s.in.a != 1.0 || s.in.b != 2 || s.in.ptr != &ptr_target) {
        return 0;
    }

    // get address of nested member
    char *char_ptr = &s.in.b;
    if (*char_ptr != 2) {
        return 0;
    }

    // dereference nested member
    *s.in.ptr = 6;
    if (ptr_target != 6) {
        return 0;
    }

    // copy one member to another
    s.in.a = s.in.b;
    if (s.in.a != 2.0) {
        return 0;
    }

    return 1;  // success
}

int test_auto_arrow(void) {
    // Test nested access in struct with automatic storage duration,
    // using only -> operator

    struct inner in;
    struct outer s;
    struct outer *s_ptr = &s;
    s_ptr->in_ptr = &in;

    // initialize non-nested members to make sure we don't overwrite them
    s_ptr->l = 4294967295ul;
    s_ptr->bar = -5;

    // writes through nested accesses
    s_ptr->in_ptr->a = 10.0;
    s_ptr->in_ptr->b = 'x';

    // this writes to s_ptr->in_array[0].a b/c of array decay
    s_ptr->in_array->a = 11.0;

    // this writes to s_ptr->in_array[3].a b/c of array decay
    (s_ptr->in_array + 3)->a = 12.0;

    // tricky: this points to int in outer struct!
    s_ptr->in_array->ptr = &s_ptr->bar;

    // make sure write didn't overwrite neighboring values
    if (s_ptr->l != 4294967295ul || s_ptr->bar != -5) {
        return 0;
    }

    // read through nested accesses
    if (s_ptr->in_ptr->a != 10.0 || s_ptr->in_ptr->b != 'x' ||
        s_ptr->in_array->a != 11.0 || (s_ptr->in_array + 3)->a != 12.0) {
        return 0;
    }

    // get address of nested member
    char *char_ptr = &s_ptr->in_ptr->b;
    if (*char_ptr != 'x') {
        return 0;
    }

    // dereference nested member
    *s_ptr->in_array->ptr = 123;  // indirectly updates s_ptr->bar
    if (s_ptr->bar != 123) {
        return 0;
    }

    // copy one member to another
    s_ptr->in_array->b = s_ptr->in_ptr->b;
    if (s_ptr->in_array[0].b != 'x') {
        return 0;
    }

    return 1;  // success
}

int test_static_arrow(void) {
    // Test nested access in struct with static storage duration,
    // using only -> operator

    static struct inner in;
    static struct outer s;

    // shouldn't really matter if this pointer is static
    static struct outer *s_ptr;
    s_ptr = &s;

    s_ptr->in_ptr = &in;

    // initialize non-nested members to make sure we don't overwrite them
    s_ptr->l = 4294967295ul;
    s_ptr->bar = -5;

    // writes through nested accesses
    s_ptr->in_ptr->a = 10.0;
    s_ptr->in_ptr->b = 'x';

    // this writes to s_ptr->in_array[0].a b/c of array decay
    s_ptr->in_array->a = 11.0;

    // this writes to s_ptr->in_array[3].a b/c of array decay
    (s_ptr->in_array + 3)->a = 12.0;

    // tricky: this points to int in outer struct!
    s_ptr->in_array->ptr = &s_ptr->bar;

    // make sure write didn't overwrite neighboring values
    if (s_ptr->l != 4294967295ul || s_ptr->bar != -5) {
        return 0;
    }

    // read through nested accesses
    if (s_ptr->in_ptr->a != 10.0 || s_ptr->in_ptr->b != 'x' ||
        s_ptr->in_array->a != 11.0 || (s_ptr->in_array + 3)->a != 12.0) {
        return 0;
    }

    // get address of nested member
    char *char_ptr = &s_ptr->in_ptr->b;
    if (*char_ptr != 'x') {
        return 0;
    }

    // dereference nested member
    *s_ptr->in_array->ptr = 123;  // indirectly updates s_ptr->bar
    if (s_ptr->bar != 123) {
        return 0;
    }

    // copy one member to another
    s_ptr->in_ptr->b = s_ptr->in_ptr->a;
    if (s_ptr->in_ptr->b != 10) {
        return 0;
    }

    return 1;  // success
}

int test_mixed(void) {
    // Test nested access using a mix of ., ->, and []
    // include: x->y.z, x.y->z, x->y[i].z
    struct inner *in_ptr = malloc(sizeof(struct inner));
    struct outer out;
    out.in_ptr = in_ptr;
    struct outer *out_ptr = &out;

    // non-nested writes to make sure these don't get clobbered
    out.l = 10;
    out.bar = 20;

    // nested writes
    out.in_ptr->a = -1.0;
    out.in_ptr->b = '!';
    out.in_ptr->ptr = 0;  // null pointer

    // nested writes thru out_ptr
    out_ptr->in_array[0].a = -2.0;
    out_ptr->in_array[0].b = '?';
    out_ptr->in_array[0].ptr = 0;  // null pointer
    // don't bother with array elements 1 and 2, skip to last one
    out_ptr->in_array[3].a = -3.0;
    out_ptr->in_array[3].b = '*';
    out_ptr->in_array[3].ptr = malloc(sizeof(int));

    out_ptr->in.a = -3.0;
    out_ptr->in.b = '&';
    int i = 9;
    out_ptr->in.ptr = &i;

    // make sure we didn't overwrite out.l or out.bar
    if (out.l != 10 || out.bar != 20) {
        return 0;
    }

    // reads via nested accesses thru out
    if (out.in_ptr->a != -1.0 || out.in_ptr->b != '!' || out.in_ptr->ptr) {
        return 0;
    }

    // reads via nested access thru out_ptr
    if (out_ptr->in_array[0].a != -2.0 || out_ptr->in_array[0].b != '?' ||
        out_ptr->in_array[0].ptr || out_ptr->in_array[3].a != -3.0 ||
        out_ptr->in_array[3].b != '*' || out_ptr->in.a != -3.0 ||
        out_ptr->in.b != '&' || out_ptr->in.ptr != &i) {
        return 0;
    }

    // dereference nested member
    *out_ptr->in_array[3].ptr = 5;
    if (*out_ptr->in_array[3].ptr != 5) {
        return 0;
    }

    // copy one member to another
    out_ptr->in.b = out.in_ptr->b;
    if (out_ptr->in.b != out.in_ptr->b) {
        return 0;
    }

    return 1;  // success
}

int test_array_of_structs(void) {
    // test nested access to array of structs using a mix of ., ->, and []
    // including x[i].y->z, x[i].y.z, x[i].y[i].z

    static struct outer struct_array[3];
    struct inner *in_ptr = malloc(sizeof(struct inner));

    // tricky: make struct_array[0].in_ptr and struct_array[1].in_ptr point to
    // same struct
    struct_array[0].in_ptr = in_ptr;
    struct_array[1].in_ptr = in_ptr;

    // write through nested access
    struct_array[0].in_ptr->a = 20.0;
    struct_array[1].in_ptr->b = '@';
    struct_array[0].in_ptr->ptr = 0;

    struct_array[1].in_array[1].a = 30.0;
    struct_array[1].in_array[0].b = '#';

    struct_array[2].in.a = 40.0;
    struct_array[2].in.b = '$';

    // read through nested access

    // if we wrote a member through struct_array[0].in_ptr,
    // read it thorugh struct_array[1].in_ptr, and vice versa,
    // since they point to the same struct inner
    if (struct_array[1].in_ptr->a != 20.0 || struct_array[0].in_ptr->b != '@' ||
        struct_array[1].in_ptr->ptr) {
        return 0;
    }

    if (struct_array[1].in_array[1].a != 30.0 ||
        struct_array[1].in_array[0].b != '#' || struct_array[2].in.a != 40.0 ||
        struct_array[2].in.b != '$') {
        return 0;
    }

    return 1;  // success
}

int test_array_of_struct_pointers(void) {
    // test nested access to array of struct pointers
    // including x[i]->y.z, x[i]->y[i].z, x[i]->y->z

    struct outer *ptr_array[2];

    ptr_array[0] = calloc(1, sizeof(struct outer));
    ptr_array[1] = calloc(1, sizeof(struct outer));

    // populate both array elements via nested writes
    // (initialize a handful of members in each struct, not all of them)

    // start with element #1
    ptr_array[1]->in_ptr = calloc(1, sizeof(struct inner));
    ptr_array[1]->in_ptr->ptr = 0;
    ptr_array[1]->in_ptr->b = '%';
    ptr_array[1]->in_ptr->a = 876.5;

    ptr_array[1]->in_array[2].a = 1000.5;

    ptr_array[1]->in.a = 7e6;

    // then element #0
    ptr_array[0]->in_ptr = calloc(1, sizeof(struct inner));
    ptr_array[0]->in_ptr->ptr = 0;
    ptr_array[0]->in_ptr->b = '^';
    ptr_array[0]->in_ptr->a = 123.4;

    ptr_array[0]->in_array[1].b = '&';

    // tricky: make this point to another element of the same struct
    ptr_array[0]->in.ptr = &ptr_array[0]->bar;

    // write to ptr_array[0]->bar to validate we can read that value through
    // *ptr_array[0]->in.ptr
    ptr_array[0]->bar = 900;

    // read through nested access; start with element #0
    if (ptr_array[0]->in_array[1].b != '&') {
        return 0;
    }

    if (ptr_array[0]->in_ptr->a != 123.4 || ptr_array[0]->in_ptr->b != '^' ||
        ptr_array[0]->in_ptr->ptr) {
        return 0;
    }

    // then read members in element #1
    if (ptr_array[1]->in.a != 7e6) {
        return 0;
    }

    if (ptr_array[1]->in_array[2].a != 1000.5) {
        return 0;
    }

    if (ptr_array[1]->in_ptr->a != 876.5 || ptr_array[1]->in_ptr->b != '%' ||
        ptr_array[1]->in_ptr->ptr) {
        return 0;
    }

    // dereference nested member
    if (*ptr_array[0]->in.ptr != 900) {
        return 0;
    }

    // make sure any elements we didn't explicitly initialize are still 0
    // i.e. assignment didn't clobber any of them

    // in ptr_array[0]
    if (ptr_array[0]->l) {
        return 0;
    }
    for (int i = 0; i < 4; i = i + 1) {
        // ptr_array[0].in_array is all 0s except for in_array[1].b
        struct inner *elem_ptr = &ptr_array[0]->in_array[i];
        if (elem_ptr->a || elem_ptr->ptr) {
            return 0;
        }

        if (elem_ptr->b && i != 1) {
            return 0;
        }
    }

    if (ptr_array[0]->in.a || ptr_array[0]->in.b) {
        return 0;
    }

    // in ptr_array[1]
    if (ptr_array[1]->l || ptr_array[1]->bar) {
        return 0;
    }

    for (int i = 0; i < 4; i = i + 1) {
        // ptr_array[1].in_array is all 0s except for in_array[2].a
        struct inner *elem_ptr = &ptr_array[1]->in_array[i];
        if (elem_ptr->b || elem_ptr->ptr) {
            return 0;
        }

        if (elem_ptr->a && i != 2) {
            return 0;
        }
    }

    if (ptr_array[1]->in.b || ptr_array[1]->in.ptr) {
        return 0;
    }

    return 1;  // success
}

int main(void) {
    if (!test_auto_dot()) {
        return 1;
    }

    if (!test_static_dot()) {
        return 2;
    }

    if (!test_auto_arrow()) {
        return 3;
    }

    if (!test_static_arrow()) {
        return 4;
    }

    if (!test_mixed()) {
        return 5;
    }

    if (!test_array_of_structs()) {
        return 6;
    }

    if (!test_array_of_struct_pointers()) {
        return 7;
    }

    return 0;
}