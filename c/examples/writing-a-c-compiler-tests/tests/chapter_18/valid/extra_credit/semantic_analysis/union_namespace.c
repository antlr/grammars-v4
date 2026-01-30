/* Test that we treat union tags, function/variable names, and each
 * struct/union's member names as separate namespaces.
 */

// Different unions/structs can use same member names
int test_shared_member_names(void) {
    union u1 {
        int a;
    };
    union u2 {
        long l;
        double a;
    };
    struct s {
        char a[2];
    };

    union u1 var1 = {10};
    union u2 var2 = {-9223372036854775807l - 1}; // LONG_MIN
    struct s var3 = {{-1, -2}};
    if (var1.a != 10 || var2.a != -0.0 || var3.a[0] != -1) {
        return 0;
    }

    return 1;  // success
}

// you can use the same identiifer as a struct tag, member name, and variable
// name
int test_same_name_var_member_and_tag(void) {
    union u {
        int u;
    };
    union u u = {100};
    if (u.u != 100) {
        return 0;
    }

    return 1;  // success
}

// you can use the same identifier as a union tag and function name
int f(void) {
    return 10;
}

union f {
    int f;
};

int test_same_name_fun_and_tag(void) {
    union f x;
    x.f = f();
    if (x.f != 10) {
        return 0;  // fail
    }

    return 1;  // success
}

int main(void) {
    if (!test_shared_member_names()) {
        return 1;  // fail
    }

    if (!test_same_name_var_member_and_tag()) {
        return 2;  // fail
    }

    if (!test_same_name_fun_and_tag()) {
        return 3;  // fail
    }

    return 0;  // success
}