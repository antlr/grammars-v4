/* Test that we resolve struct tags wherever they appear:
 * In function, member, and variable declarations,
 * for loop headers, cast expressions, sizeof, derived types
 * and does nothing if one is already in scope?
 * */

void *calloc(unsigned long nmemb, unsigned long size);
void *malloc(unsigned long size);

// simple struct type used in several tests
struct s {
    int a;
};

// tag resolution in variable declarations
// (based on Listing 18-5)
int test_var_declaration(void) {
    struct shadow {
        int x;
    };
    struct shadow outer;
    outer.x = 2;
    {
        // define new struct type, shadowing first one
        struct shadow {
            int y;
        };

        // define a variable with this type
        struct shadow inner;

        // make sure we still know that outer has outer type
        // and inner has inner type, and we can access members in both
        inner.y = 3;
        if (outer.x != 2) {
            return 0;
        }

        if (inner.y != 3) {
            return 0;
        }
    }

    return 1;  // success
}

// tag resolution in struct member declaration
int test_member_declaration(void) {
    struct s {
        int b;
        // this specifies a pointer to the "struct s" type
        // we're currently defining, not the type declared at file scope
        struct s *self_ptr;
    };

    struct s my_struct = {123, 0};

    // make sure we've inferred the correct type for self_ptr by
    // assigning to it and then accessing a member through it
    my_struct.self_ptr = &my_struct;
    if (my_struct.self_ptr->b != 123) {
        return 0;
    }

    return 1;  // success
}

// tag resolution in function declaration
int test_function_declaration(void) {
    // this has struct type declared at file scope
    struct s outer_struct = {1};
    {
        // shadow the file-scope declaration
        struct s {
            int arr[40];
        };
    }

    // tag resolution in function declaration:
    // now that inner 'struct s' is out of scope,
    // declaration will refer to out one
    struct s *copy_struct(struct s * arg);

    // make sure we can call this function
    struct s *copy = copy_struct(&outer_struct);
    if (copy->a != outer_struct.a) {
        return 0;
    }

    return 1;  // success
}

struct s *copy_struct(struct s *arg) {
    struct s *ptr = malloc(4);
    ptr->a = arg->a;
    return ptr;
}

// tag resolution in for loops
int test_for_loop(void) {
    // make sure we can declare variables of structure type in for loop headers
    for (struct s loop_struct = {10}; loop_struct.a > 0;
         loop_struct.a = loop_struct.a - 1) {
        // this is a new scope, make sure we can define a new struct s here
        struct s {
            double d;
        };
        static struct s loop_body_struct = {0};

        // make sure we know the types of both structs
        loop_body_struct.d = loop_body_struct.d + 1;

        if (loop_struct.a == 1) {
            // last iteration
            if (loop_body_struct.d != 10.0) {
                return 0;
            }
        }
    }

    return 1;  // success
}

// tag resolution in cast expressions
int test_cast(void) {
    void *ptr = malloc(10);

    if (ptr) {
        struct s {
            char arr[10];
        };

        // we can cast to inner struct type
        ((struct s *)ptr)->arr[2] = 10;

        // examine struct as char array to make sure assignment worked
        char byte = ((char *)ptr)[2];
        if (byte != 10) {
            return 0;
        }
    }

    // back out of scope, 'struct s' refers to file scope struct again
    void *second_ptr = malloc(4);

    ((struct s *)second_ptr)->a = 10;
    char lowest_byte = ((char *)second_ptr)[0];
    if (lowest_byte != 10) {
        return 0;
    }

    return 1;  // success
}

// tag resolution in sizeof expressions
int test_sizeof(void) {
    struct s {
        int a;
        int b;
    };
    struct s x;  // x is an eight-byte struct
    {
        struct s {
            char arr[15];
        };  // declare a 15-byte struct type

        // in this scope, 'x' has outer type but specifier refers to inner type
        if (sizeof x != 8) {
            return 0;
        };

        if (sizeof(struct s) != 15) {
            return 0;
        }
    }

    // now 'struct s' refers to struct declared at start of function,
    // still shadowing 'struct s' from file scope
    if (sizeof(struct s) != 8) {
        return 0;
    }

    return 1;  // success
}

// tag resolution in derived types
int test_derived_types(void) {
    struct s outer_struct = {1};

    // pointer to array of three pointers to struct s
    struct s *(*outer_arr)[3] = calloc(3, sizeof(void *));

    // declare another struct type to shadow outer one
    struct s {
        int x;
    };

    struct s inner_struct = {2};

    // pointer to array of three pointers to inner struct s
    struct s *(*inner_arr)[3] = calloc(3, sizeof(void *));

    // the type checker should recognize that outer_arr[0][0] and &outer_struct
    // have the same type, so these assignments are valid
    outer_arr[0][0] = &outer_struct;
    outer_arr[0][1] = &outer_struct;

    // the type checker should recognize that inner_arr[0][0] and &inner_struct
    // have the same type, so these assignments are valid
    inner_arr[0][0] = &inner_struct;
    inner_arr[0][2] = &inner_struct;

    if (outer_arr[0][0]->a != 1) {
        return 0;
    }

    if (inner_arr[0][0]->x != 2) {
        return 0;
    }

    return 1;
}

// a struct tag declaration with no member list does nothing
// if that tag was already declared in the current scope
int test_contentless_tag_noop(void) {
    struct s {
        int x;
        int y;
    };

    struct s;

    struct s var;  // this has the type declared at the start of this function

    var.x = 10;
    var.y = 11;

    if (var.x != 10 || var.y != 11) {
        return 0;
    }

    return 1;
}

int main(void) {
    if (!test_var_declaration()) {
        return 1;
    }

    if (!test_member_declaration()) {
        return 2;
    }

    if (!test_function_declaration()) {
        return 3;
    }

    if (!test_for_loop()) {
        return 4;
    }

    if (!test_cast()) {
        return 5;
    }

    if (!test_sizeof()) {
        return 6;
    }

    if (!test_derived_types()) {
        return 7;
    }

    if (!test_contentless_tag_noop()) {
        return 8;
    }

    return 0;  // success
}
