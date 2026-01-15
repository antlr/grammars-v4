/* Test that returning a struct doesn't clobber the stack.
 * This is most likely when we're returning structs in memory, but test other
 * types of structs too.
 * To test this, we store some bytes on the stack, call a function that returns
 * the struct, then validate that those bytes haven't changed. In the functions
 * whose stacks we validate, we don't store any values on the stack except the
 * bytes to validate and the return value. This ensures that if the return
 * value clobbers any other bytes on the stack, we'll detect it. This is a
 * similar technique to
 * chapter_18/valid/no_structure_parameters/struct_copy/stack_clobber.c
 * This test assumes structures are allocated on the stack in the same order
 * they're declared/initialized (otherwise clobbers may overwrite stack padding
 * instead of data that we validate, and go undetected).
 */

#ifdef SUPPRESS_WARNINGS
#ifdef __clang__
#pragma clang diagnostic ignored "-Wincompatible-library-redeclaration"
#else
#pragma GCC diagnostic ignored "-Wbuiltin-declaration-mismatch"
#endif
#endif

int strcmp(char *s1, char *s2);
void exit(int status);

struct stack_bytes {
    char bytes[16];
};

// we copy bytes from the stack to here, then validate them
static struct stack_bytes to_validate;

// use this to validate to_validate after copying bytes from stack to it
void validate_stack_bytes(int code) {
    if (strcmp(to_validate.bytes, "efghijklmnopqrs")) {
        exit(code);
    }
    return;
}

// test case 1: return a struct in a general-purpose register
struct one_int_reg {
    char cs[7];
};

struct one_int_reg return_int_struct(void) {
    struct one_int_reg retval = {{0, 0, 0, 0, 0, 0, 0}};
    return retval;
}

static struct one_int_reg one_int_struct;
void validate_one_int_struct(int code) {
    for (int i = 0; i < 7; i = i + 1) {
        if (one_int_struct.cs[i]) {
            exit(code);
        }
    }
}

int test_int_struct(void) {
    // write some bytes to the stack
    struct stack_bytes bytes = {"efghijklmnopqrs"};

    // call a function that returns a one-int struct
    // copy it to a static variable so we can validate it later
    // without putting more temporary variables on the satck
    one_int_struct = return_int_struct();

    // assigning a variable doesn't produce any temporary values
    to_validate = bytes;

    // this funcall doesn't require temporary values on the stack
    // b/c its arg is just an int(not a more complex expression)
    // and its return type
    validate_stack_bytes(1);

    /// validate the static struct we copied the return val into earlier
    validate_one_int_struct(2);
    return 0;
}

// test case 2: return a struct in two general-purpose registers
struct two_int_regs {
    char cs[15];
};

struct two_int_regs return_two_int_struct(void) {
    struct two_int_regs retval = {
        {20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 33, 34}};
    return retval;
}

static struct two_int_regs two_int_struct;
void validate_two_int_struct(int code) {
    for (int i = 0; i < 15; i = i + 1)
        if (two_int_struct.cs[i] != i + 20) {
            exit(code);
        }
}

int test_two_int_struct(void) {
    // write some bytes to the stack
    struct stack_bytes bytes = {"efghijklmnopqrs"};

    two_int_struct = return_two_int_struct();

    // assigning a variable doesn't produce any temporary values
    to_validate = bytes;

    // validate stack
    validate_stack_bytes(3);

    /// validate returned struct
    validate_two_int_struct(4);
    return 0;
}

// test case 3: return a struct in one XMM register
struct one_xmm_reg {
    double d;
};

struct one_xmm_reg return_one_xmm_struct(void) {
    struct one_xmm_reg retval = {234.5};
    return retval;
}

static struct one_xmm_reg one_double_struct;
void validate_one_double_struct(int code) {
    if (one_double_struct.d != 234.5) {
        exit(code);
    }
}

int test_one_double_struct(void) {
    // write some bytes to the stack
    struct stack_bytes bytes = {"efghijklmnopqrs"};

    one_double_struct = return_one_xmm_struct();

    // assigning a variable doesn't produce any temporary values
    to_validate = bytes;

    // validate stack
    validate_stack_bytes(5);

    /// validate returned struct
    validate_one_double_struct(6);
    return 0;
}

// test case 4: return a struct in two XMM registers
struct two_xmm_regs {
    double d1;
    double d2;
};

struct two_xmm_regs return_two_xmm_struct(void) {
    struct two_xmm_regs retval = {234.5, 678.25};
    return retval;
}

static struct two_xmm_regs two_doubles_struct;
void validate_two_doubles_struct(int code) {
    if (two_doubles_struct.d1 != 234.5 || two_doubles_struct.d2 != 678.25) {
        exit(code);
    }
}

int test_two_doubles_struct(void) {
    // write some bytes to the stack
    struct stack_bytes bytes = {"efghijklmnopqrs"};

    two_doubles_struct = return_two_xmm_struct();

    // assigning a variable doesn't produce any temporary values
    to_validate = bytes;

    // validate stack
    validate_stack_bytes(7);

    /// validate returned struct
    validate_two_doubles_struct(8);
    return 0;
}

// test case 5: return a stuct in general-purpose and XMM registers

struct int_and_xmm {
    char c;
    double d;
};

struct int_and_xmm return_mixed_struct(void) {
    struct int_and_xmm retval = {125, 678.25};
    return retval;
}

static struct int_and_xmm mixed_struct;
void validate_mixed_struct(int code) {
    if (mixed_struct.c != 125 || mixed_struct.d != 678.25) {
        exit(code);
    }
}

int test_mixed_struct(void) {
    // write some bytes to the stack
    struct stack_bytes bytes = {"efghijklmnopqrs"};

    mixed_struct = return_mixed_struct();

    // assigning a variable doesn't produce any temporary values
    to_validate = bytes;

    // validate stack
    validate_stack_bytes(9);

    /// validate returned struct
    validate_mixed_struct(10);
    return 0;
}

// test case 6: return a struct on the stack
struct stack {
    char cs[28];
};

struct stack return_stack_struct(void) {
    struct stack retval = {{90,  91,  92,  93,  94,  95,  96,  97,  98,  99,
                            100, 101, 102, 103, 104, 105, 106, 107, 108, 109,
                            110, 111, 112, 113, 114, 115, 116, 117}};
    return retval;
}

static struct stack stack_struct;
void validate_stack_struct(int code) {
    for (int i = 0; i < 28; i = i + 1) {
        if (stack_struct.cs[i] != i + 90) {
            exit(code);
        }
    }
}

int test_stack_struct(void) {
    // write some bytes to the stack
    struct stack_bytes bytes = {"efghijklmnopqrs"};

    stack_struct = return_stack_struct();

    // assigning a variable doesn't produce any temporary values
    to_validate = bytes;

    // validate stack
    validate_stack_bytes(11);

    /// validate returned struct
    validate_stack_struct(12);
    return 0;
}

// test case 7: return an irregularly-slized struct on the stack
struct stack_irregular {
    char cs[19];
};

struct stack_irregular return_irregular_stack_struct(void) {
    struct stack_irregular retval = {{70, 71, 72, 73, 74, 75, 76, 77, 78, 79,
                                      80, 81, 82, 83, 84, 85, 86, 87, 88}};
    return retval;
}

static struct stack_irregular irregular_stack_struct;
void validate_irregular_stack_struct(int code) {
    for (int i = 0; i < 19; i = i + 1) {
        if (irregular_stack_struct.cs[i] != i + 70) {
            exit(code);
        }
    }
}

int test_irregular_stack_struct(void) {
    // write some bytes to the stack
    struct stack_bytes bytes = {"efghijklmnopqrs"};

    irregular_stack_struct = return_irregular_stack_struct();

    // assigning a variable doesn't produce any temporary values
    to_validate = bytes;

    // validate stack
    validate_stack_bytes(13);

    /// validate returned struct
    validate_irregular_stack_struct(14);
    return 0;
}

int main(void) {
    test_int_struct();
    test_two_int_struct();
    test_one_double_struct();
    test_two_doubles_struct();
    test_mixed_struct();
    test_stack_struct();
    test_irregular_stack_struct();
    return 0;
}