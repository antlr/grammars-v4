/* Test copying whole structs via Load and Store,
 * including copies of the form *x = y , x = *y,
 * and reads and writes of array elements,
 * with a mix of static and automatic structs
 * */

#include "structs.h"

// case 1: *x = y
int test_copy_to_pointer(void) {
    struct s y = {"!?", {-20, -30}};
    struct s *x = malloc(sizeof(struct s));
    *x = y;

    // validate
    if (strcmp(x->arr, "!?") || x->inner.a != -20 || x->inner.b != -30) {
        return 0;
    }

    return 1;  // success
}

// case 2: x = *y
int test_copy_from_pointer(void) {
    static struct s my_struct = {"()", {77, 78}};
    struct s *y = &my_struct;
    struct s x = {"", {0, 0}};
    x = *y;

    // validate
    if (strcmp(x.arr, "()") || x.inner.a != 77 || x.inner.b != 78) {
        return 0;
    }

    return 1;  // success
}

// case 3: *x = *y
int test_copy_to_and_from_pointer(void) {
    struct s my_struct = {"+-", {1000, 1001}};
    struct s *y = &my_struct;
    struct s *x = malloc(sizeof(struct s));
    *x = *y;

    // validate
    if (strcmp(x->arr, "+-") || x->inner.a != 1000 || x->inner.b != 1001) {
        return 0;
    }

    return 1;  // success
}

// case 4: arr[i] = y
int test_copy_to_array_elem(void) {
    struct s y = {"\n\t", {10000, 20000}};
    static struct s arr[3];

    arr[1] = y;

    // validate
    if (strcmp(arr[1].arr, "\n\t") || arr[1].inner.a != 10000 ||
        arr[1].inner.b != 20000) {
        return 0;
    }

    // make sure adjoining array elements are unchanged
    if (arr[0].inner.a || arr[0].inner.b || arr[2].arr[0] || arr[2].arr[1]) {
        return 0;
    }
    return 1;  // success
}

// case 5: x = arr[i]
int test_copy_from_array_elem(void) {
    struct s arr[3] = {
        {"ab", {-3000, -4000}}, {"cd", {-5000, -6000}}, {"ef", {-7000, -8000}}};

    struct s x = {"", {0, 0}};
    x = arr[1];
    // validate
    if (strcmp(x.arr, "cd") || x.inner.a != -5000 || x.inner.b != -6000) {
        return 0;
    }

    return 1;  // success
}

// case 6: arr[i] = arr[j]
int test_copy_to_and_from_array_elem(void) {
    struct s arr[3] = {
        {"ab", {-3000, -4000}}, {"cd", {-5000, -6000}}, {"ef", {-7000, -8000}}};

    arr[0] = arr[2];
    // validate all elements

    // element 0
    if (strcmp(arr[0].arr, "ef") || arr[0].inner.a != -7000 ||
        arr[0].inner.b != -8000) {
        return 0;
    }

    // element 1
    if (strcmp(arr[1].arr, "cd") || arr[1].inner.a != -5000 ||
        arr[1].inner.b != -6000) {
        return 0;
    }

    // element 2
    if (strcmp(arr[2].arr, "ef") || arr[2].inner.a != -7000 ||
        arr[2].inner.b != -8000) {
        return 0;
    }

    return 1;  // success
}

// case 7: copy struct w/ trailing padding to array element
int test_copy_array_element_with_padding(void) {
    struct with_end_padding arr[3] = {{0, 1, 2}, {3, 4, 5}, {6, 7, 8}};
    struct with_end_padding elem = {9, 9, 9};
    arr[1] = elem;
    if (arr[0].a != 0 || arr[0].b != 1 || arr[0].c != 2 || arr[1].a != 9 ||
        arr[1].b != 9 || arr[1].c != 9 || arr[2].a != 6 || arr[2].b != 7 ||
        arr[2].c != 8) {
        return 0;
    }

    return 1;  // success
}

int main(void) {
    if (!test_copy_to_pointer()) {
        return 1;
    }

    if (!test_copy_from_pointer()) {
        return 2;
    }

    if (!test_copy_to_and_from_pointer()) {
        return 3;
    }
    if (!test_copy_to_array_elem()) {
        return 4;
    }
    if (!test_copy_from_array_elem()) {
        return 5;
    }

    if (!test_copy_to_and_from_array_elem()) {
        return 6;
    }

    if (!test_copy_array_element_with_padding()) {
        return 7;
    }
    return 0;  // success
}