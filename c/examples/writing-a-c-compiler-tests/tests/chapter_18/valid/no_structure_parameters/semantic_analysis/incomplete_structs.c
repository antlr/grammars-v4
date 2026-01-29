/* Test that our typechecker can handle valid declarations and expressions
 * involving incomplete structure types
 * */

#ifdef SUPPRESS_WARNINGS
#ifdef __clang__
#pragma clang diagnostic ignored "-Wincompatible-library-redeclaration"
#else
#pragma GCC diagnostic ignored "-Wbuiltin-declaration-mismatch"
#endif
#endif

void *malloc(unsigned long size);
void *calloc(unsigned long nmemb, unsigned long size);
int puts(char *s);
int strcmp(char *s1, char *s2);

// test 1: you can declare a function that accepts/returns incomplete struct
// types. We don't define or use this function, we just need to validate that
// this declaration doesn't cause a compiler error
struct never_used;
struct never_used incomplete_fun(struct never_used x);

// test 2: you can declare an incomplete struct type at block scope,
// then complete it
int test_block_scope_forward_decl(void) {
    struct s;             // declare incomplete struct type
    struct s *s_ptr = 0;  // define a pointer to that struct type

    struct s {
        int x;
        int y;
    };  // complete the type

    // now you can use s_ptr as a pointer to a completed type
    struct s val = {1, 2};
    s_ptr = &val;
    if (s_ptr->x != 1 || s_ptr->y != 2) {
        return 0;
    }

    return 1;  // success
}

// test 3: you can declare an incomplete struct type at file scope,
// then complete it
struct pair;  // declare an incomplete type

// declare functions involving pointers to that type
struct pair *make_struct(void);
int validate_struct(struct pair *ptr);

int test_file_scope_forward_decl(void) {
    // call the functions
    struct pair *my_struct = make_struct();
    return validate_struct(my_struct);
    // this case validates by printing to stdout, not w/ return coe
}

// complete the type
struct pair {
    long l;
    long m;
};

// define the functions
struct pair *make_struct(void) {
    struct pair *retval = malloc(sizeof(struct pair));
    retval->l = 100;
    retval->m = 200;
    return retval;
}

int validate_struct(struct pair *ptr) {
    return (ptr->l == 100 && ptr->m == 200);
}

// test 4: you can declare and take the address of,
// but not define or use, variables with incomplete type

struct msg_holder;
void print_msg(struct msg_holder *param);
int validate_incomplete_var(void);

// okay to declare extern variable w/ incomplete type
extern struct msg_holder incomplete_var;

int test_incomplete_var(void) {
    // okay to take address of incomplete var
    print_msg(&incomplete_var);
    return validate_incomplete_var();
}

// complete the type
struct msg_holder {
    char *msg;
};

// now we can use value of incomplete_var
int validate_incomplete_var(void) {
    if (strcmp(incomplete_var.msg, "I'm a struct!")) {
        return 0;
    }

    return 1;  // succes
}

// and we can define it
struct msg_holder incomplete_var = {"I'm a struct!"};

// also need to define print_msg
void print_msg(struct msg_holder *param) {
    puts(param->msg);
}

// test 5: you can dereference a pointer to an incomplete var, then take its
// address
int test_deref_incomplete_var(void) {
    struct undefined_struct;
    struct undefined_struct *ptr = malloc(4);
    // NOTE: GCC fails to compile this before version 10
    // see https://gcc.gnu.org/bugzilla/show_bug.cgi?id=88827
    return &*ptr == ptr;
}

// test 6: more things you can do with pointers to incomplete structs:
// return pointers to them, accept them as parameters, use them in conditionals,
// cast them to void * or char *
// compare them to 0 and each other
struct opaque_struct;

struct opaque_struct *use_struct_pointers(struct opaque_struct *param) {
    if (param == 0) {
        puts("empty pointer!");
    }
    return 0;
}

int test_use_incomplete_struct_pointers(void) {
    // define a couple of pointers to this type
    struct opaque_struct *ptr1 = calloc(1, 4);
    struct opaque_struct *ptr2 = calloc(1, 4);

    // can cast to char * and inspect; this is well-defined
    // and all bits should be 0 since we used calloc
    char *ptr1_bytes = (char *)ptr1;
    if (ptr1_bytes[0] || ptr1_bytes[1]) {
        return 0;
    }

    // can compare to 0 or each other
    if (ptr1 == 0 || ptr2 == 0 || ptr1 == ptr2) {
        return 0;
    }

    // can use them in conditionals
    static int flse = 0;
    struct opaque_struct *ptr3 = flse ? ptr1 : ptr2;
    if (ptr3 != ptr2) {
        return 0;
    }

    // can pass them as parameters
    if (use_struct_pointers(ptr3)) {
        return 0;
    }

    return 1;  // success
}

int main(void) {
    if (!test_block_scope_forward_decl()) {
        return 2;
    }

    if (!test_file_scope_forward_decl()) {
        return 3;
    }

    if (!test_incomplete_var()) {
        return 4;
    }

    if (!test_deref_incomplete_var()) {
        return 5;
    }

    if (!test_use_incomplete_struct_pointers()) {
        return 6;
    }

    return 0;  // success
}