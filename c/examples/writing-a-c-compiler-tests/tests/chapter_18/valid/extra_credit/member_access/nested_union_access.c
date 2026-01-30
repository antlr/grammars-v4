/* Test access to nested union members through dot, arrow, and subscript operators */

#include "../union_types.h"

int test_auto_dot(void) {
    // Test nested access with . in unions/structs containing unions
    // with automatic storage duration

    // access union in union
    union has_union x;
    x.u.l = 200000u;
    if (x.u.i != 200000) {
        return 0; // fail
    }

    // access struct in union
    union has_struct y;
    y.s.l = -5555l;
    y.s.d = 10.0;
    y.s.u = 100;

    if (y.l != -5555l) {
        return 0; // fail
    }

    // access union in struct in union
    union complex_union z;
    z.s.u.i = 12345;
    z.s.ul = 0;

    if (z.s.u.c != 57) { // lowest byte of 12345
        return 0; // fail
    }

    if (z.d_arr[1]) { // bytes 8-15 of  union; same spot as z.s.ul
        return 0; // fail
    }

    // get/derefrence address of various members
    unsigned int *some_int_ptr = &y.s.u;
    union simple *some_union_ptr = &z.s.u;

    if (*some_int_ptr != 100 || (*some_union_ptr).i != 12345) {
        return 0; // fail
    }

    return 1; // success
}

int test_static_dot(void) {
    // identical to test_auto_dot but using objects
    // with static storage duration

    // access union in union
    static union has_union x;
    x.u.l = 200000u;
    if (x.u.i != 200000) {
        return 0; // fail
    }

    // access struct in union
    static union has_struct y;
    y.s.l = -5555l;
    y.s.d = 10.0;
    y.s.u = 100;

    if (y.l != -5555l) {
        return 0; // fail
    }

    // access union in struct in union
    static union complex_union z;
    z.s.u.i = 12345;
    z.s.ul = 0;

    if (z.s.u.c != 57) { // lowest byte of 12345
        return 0; // fail
    }

    if (z.d_arr[1]) { // bytes 8-15 of  union; same spot as z.s.ul
        return 0; // fail
    }

    return 1; // success
}

int test_auto_arrow(void) {
    // Test nested access in unions w/ automatic storage duration,
    // using only -> operator
    union simple inner = {100};
    union has_union outer;
    union has_union *outer_ptr = &outer;
    outer_ptr->u_ptr = &inner;
    if (outer_ptr->u_ptr->i != 100) {
        return 0; // fail
    }

    // write through nested access
    outer_ptr->u_ptr->l = -10;

    // read through other members that should have same value
    if (outer_ptr->u_ptr->c != -10 || outer_ptr->u_ptr->i != -10 || outer_ptr->u_ptr->l != -10) {
        return 0; // fail
    }

    // read through members of uc_arr
    if (outer_ptr->u_ptr->uc_arr[0] != 246 || outer_ptr->u_ptr->uc_arr[1] != 255 || outer_ptr->u_ptr->uc_arr[2] != 255) {
        return 0; // fail
    }

    return 1; // success
}

int test_static_arrow(void) {
    // identical to test_auto_arrow but with objects of static storage duration
    static union simple inner = {100};
    static union has_union outer;
    static union has_union *outer_ptr;
    outer_ptr = &outer;
    outer_ptr->u_ptr = &inner;
    if (outer_ptr->u_ptr->i != 100) {
        return 0; // fail
    }

    // write through nested access
    outer_ptr->u_ptr->l = -10;

    // read through other members that should have same value
    if (outer_ptr->u_ptr->c != -10 || outer_ptr->u_ptr->i != -10 || outer_ptr->u_ptr->l != -10) {
        return 0; // fail
    }

    // read through members of uc_arr
    if (outer_ptr->u_ptr->uc_arr[0] != 246 || outer_ptr->u_ptr->uc_arr[1] != 255 || outer_ptr->u_ptr->uc_arr[2] != 255) {
        return 0; // fail
    }

    return 1; // success
}

int test_array_of_unions(void) {
    // test access to array of unions
    union has_union arr[3];
    arr[0].u.l = -10000;
    arr[1].u.i = 200;
    arr[2].u.c = -120;

    if (arr[0].u.l != -10000 || arr[1].u.c != -56 || arr[2].u.uc_arr[0] != 136) {
        return 0; // fail
    }

    return 1; // success
}

int test_array_of_union_pointers(void) {
    // test access to array of union pointers
    union has_union *ptr_arr[3];
    for (int i = 0; i < 3; i = i + 1) {
        ptr_arr[i] = calloc(1, sizeof(union has_union));
        ptr_arr[i]->u_ptr = calloc(1, sizeof (union simple));
        ptr_arr[i]->u_ptr->l = i;
    }

    if (ptr_arr[0]->u_ptr->l != 0 || ptr_arr[1]->u_ptr->l != 1 || ptr_arr[2]->u_ptr->l != 2) {
        return 0; // fail
    }

    return 1;
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

    if (!test_array_of_unions()) {
        return 5;
    }

    if (!test_array_of_union_pointers()) {
        return 6;
    }

    return 0;
}