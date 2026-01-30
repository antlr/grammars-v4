/* You can't declare 'struct s' and define 'union s' or vice versa in same scope
 * (This test is intended to verify that we detect conflicts between declarations
 * and definitions rather than just ignoring the declarations/letting the
 * definitions overwrite them; identical to type_decl_conflicts_with_def with
 * order of type declarations swapped)
 * */

int main(void) {
    union s { // conflicts w/ earlier declaration
        int a;
    };
    struct s;

    return 0;
}