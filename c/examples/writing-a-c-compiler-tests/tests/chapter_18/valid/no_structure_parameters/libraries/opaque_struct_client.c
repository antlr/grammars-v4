/* Test working with a structure whose type is completed in the library but not
 * the client; this is a common idiom for hiding a library's implementation
 * details */

struct s;  // declare a type but don't define it

// declare some functions for getting/using pointers to this type
struct s *create_struct(int i, double d, char *s);
void increment_struct(struct s *src_ptr);

int check_struct(struct s *ptr, int expected_i, double expected_d,
                 char *expected_s);
void print_struct_msg(struct s *ptr);
struct s *get_internal_struct(void);

// declare a variable with this type
extern struct s incomplete_var;

int main(void) {
    struct s *new_struct = create_struct(101, 102.0, "new struct");

    struct s *internal_struct = get_internal_struct();

    // print initial message from all three structs
    print_struct_msg(new_struct);
    print_struct_msg(internal_struct);
    print_struct_msg(&incomplete_var);

    // modify some structs
    increment_struct(new_struct);
    increment_struct(&incomplete_var);

    // check values
    if (!check_struct(new_struct, 102, 103.0, "new struct")) {
        return 1;
    }

    if (!check_struct(&incomplete_var, 4, 5.0, "global struct")) {
        return 2;
    }

    return 0;  // success (assuming stdout is also correct)
}