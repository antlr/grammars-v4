/* Test that we treat struct tags, function/variable names,
 * and each struct type's member names as separate namespaces
 * */

// two structs can use same member names
int test_shared_member_names(void) {
    struct pair1 {
        int x;
        int y;
    };

    struct pair2 {
        double x;
        char y;
    };
    struct pair1 p1 = {1, 2};
    struct pair2 p2 = {3.0, 4};
    if (p1.x != 1 || p2.x != 3.0) {
        return 0;
    }

    return 1;  // success
}

// you can use the same member name in different ways in the same expression,
// and the type checker can infer what struct type each one refers to
int test_shared_nested_member_names(void) {
    struct pair1 {
        int x;
        int *y;
    };

    struct pair2 {
        void *x;
        double y[4];
    };
    struct pair1 p1 = {3, &(p1.x)};
    struct pair2 p2 = {&p1, {1.0, 2.0, 3.0, 4.0}};

    // nested access with two 'y' members
    if (((struct pair1 *)p2.x)->x != 3) {
        return 0;
    }

    return 1;  // success
}

// you can use the same identiifer as a struct tag, member name, and variable
// name
int test_same_name_var_member_and_tag(void) {
    struct x {
        int x;
    };
    struct x x = {10};
    if (x.x != 10) {
        return 0;
    }

    return 1;  // success
}

// you can use the same identifier as a struct tag, member name, and function
// name
int test_same_name_fun_member_and_tag(void) {
    struct f {
        int f;
    };
    int f(void);
    struct f my_struct;
    my_struct.f = f();
    if (my_struct.f != 10) {
        return 0;
    }

    return 1;  // success
}

int f(void) {
    return 10;
}

int main(void) {
    if (!test_shared_member_names()) {
        return 1;
    }

    if (!test_shared_nested_member_names()) {
        return 2;
    }

    if (!test_same_name_var_member_and_tag()) {
        return 3;
    }

    if (!test_same_name_fun_member_and_tag()) {
        return 4;
    }

    return 0;
}