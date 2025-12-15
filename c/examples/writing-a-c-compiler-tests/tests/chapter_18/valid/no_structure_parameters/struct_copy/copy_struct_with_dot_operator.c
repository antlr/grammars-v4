// Test using . to copy entire structures

struct inner {
    signed char a;
    signed char b;
    signed char arr[3];
};

struct outer {
    struct inner substruct;
    signed char x;
    signed char y;
};

struct outermost {
    struct outer nested;
    int i;
};

// case 1: x = y.z
int test_copy_from_member(void) {
    static struct outer big_struct = {{10, 9, {8, 7, 6}}, 5, 4};
    // allocate other objects on the stack around substruct, make sure they
    // aren't overwritten
    char arr[3] = {'a', 'b', 'c'};
    struct inner substruct = {-1, -1, {-1, -1, -1}};
    char arr2[3] = {'d', 'e', 'f'};

    substruct = big_struct.substruct;

    // validate substruct
    if (substruct.a != 10 || substruct.b != 9 || substruct.arr[0] != 8 ||
        substruct.arr[1] != 7 || substruct.arr[2] != 6) {
        return 0;
    }

    // validate other objects on the stack
    if (arr[0] != 'a' || arr[1] != 'b' || arr[2] != 'c' || arr2[0] != 'd' ||
        arr2[1] != 'e' || arr2[2] != 'f') {
        return 0;
    }

    return 1;  // success
}

// case 2: x.y = z
int test_copy_to_member(void) {
    static struct outer big_struct = {{0, 0, {0, 0, 0}}, 0, 0};
    struct inner small_struct = {-1, -2, {-3, -4, -5}};
    big_struct.substruct = small_struct;

    // make sure we updated substruct w/out overwriting other members
    if (big_struct.substruct.a != -1 || big_struct.substruct.b != -2 ||
        big_struct.substruct.arr[0] != -3 ||
        big_struct.substruct.arr[1] != -4 ||
        big_struct.substruct.arr[2] != -5) {
        return 0;
    }

    if (big_struct.x || big_struct.y) {
        return 0;
    }

    return 1;  // success
}

// case 3: a = x.y.z
int test_copy_from_nested_member(void) {
    struct outermost biggest_struct = {{{-1, -2, {-3, -4, -5}}, -6, -7}, 0};
    static struct inner small_struct;

    small_struct = biggest_struct.nested.substruct;

    if (small_struct.a != -1 || small_struct.b != -2 ||
        small_struct.arr[0] != -3 || small_struct.arr[1] != -4 ||
        small_struct.arr[2] != -5) {
        return 0;
    }

    return 1;  // success
}

// case 4: x.y.z = a
int test_copy_to_nested_member(void) {
    struct outermost biggest_struct = {{{0, 0, {0, 0, 0}}, 0, 0}, -1};
    static struct inner small_struct = {50, 51, {52, 53, 54}};
    biggest_struct.nested.substruct = small_struct;

    if (biggest_struct.nested.substruct.a != 50 ||
        biggest_struct.nested.substruct.b != 51 ||
        biggest_struct.nested.substruct.arr[0] != 52 ||
        biggest_struct.nested.substruct.arr[1] != 53 ||
        biggest_struct.nested.substruct.arr[2] != 54) {
        return 0;
    }

    if (biggest_struct.nested.x || biggest_struct.nested.y) {
        return 0;
    }

    if (biggest_struct.i != -1) {
        return 0;
    }

    return 1;  // success
}

// case 5: a = (flag ? x : y).z
int test_copy_from_conditional(void) {
    struct outer big_struct = {{127, -128, {61, 62, 63}}, -10, -11};
    struct outer big_struct2 = {{0, 1, {2, 3, 4}}, 5, 6};
    static int t = 1;
    static int f = 0;

    // get member from conditional expression where controlling expression is
    // false
    struct inner small_struct = (f ? big_struct : big_struct2).substruct;

    // validate
    if (small_struct.a != 0 || small_struct.b != 1 ||
        small_struct.arr[0] != 2 || small_struct.arr[1] != 3 ||
        small_struct.arr[2] != 4) {
        return 0;
    }
    // get member from conditional expression where controlling expression is
    // true
    small_struct = (t ? big_struct : big_struct2).substruct;

    // validate
    if (small_struct.a != 127 || small_struct.b != -128 ||
        small_struct.arr[0] != 61 || small_struct.arr[1] != 62 ||
        small_struct.arr[2] != 63) {
        return 0;
    }

    return 1;  // success
}

// case 6: a = (x = y).z
int test_copy_from_assignment(void) {
    struct outer big_struct = {{127, -128, {61, 62, 63}}, -10, -11};
    static struct outer big_struct2;

    static struct inner small_struct;

    // get member from assignment statement
    small_struct = (big_struct2 = big_struct).substruct;

    // validate result of member expression
    if (small_struct.a != 127 || small_struct.b != -128 ||
        small_struct.arr[0] != 61 || small_struct.arr[1] != 62 ||
        small_struct.arr[2] != 63) {
        return 0;
    }

    // validate that we actually performed assignment

    if (big_struct2.substruct.a != 127 || big_struct2.substruct.b != -128 ||
        big_struct2.substruct.arr[0] != 61 ||
        big_struct2.substruct.arr[1] != 62 ||
        big_struct2.substruct.arr[2] != 63 || big_struct2.x != -10 ||
        big_struct2.y != -11) {
        return 0;
    }

    return 1;  // success
}

int main(void) {
    if (!test_copy_from_member()) {
        return 1;
    }

    if (!test_copy_to_member()) {
        return 2;
    }

    if (!test_copy_from_nested_member()) {
        return 3;
    }

    if (!test_copy_to_nested_member()) {
        return 4;
    }

    if (!test_copy_from_conditional()) {
        return 6;
    }

    if (!test_copy_from_assignment()) {
        return 7;
    }

    return 0;  // success
}