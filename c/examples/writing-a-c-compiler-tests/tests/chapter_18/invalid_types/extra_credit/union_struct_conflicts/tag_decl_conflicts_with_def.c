/* You can't declare 'struct s' and define 'union s' or vice versa in same scope
 * (This test is intended to verify that we detect conflicts between declarations
 * and definitions rather than just ignoring the declarations/letting the
 * definitions overwrite them)
 * */

int main(void) {
    struct s;
    union s { // conflicts w/ earlier declaration
        int a;
    };

    return 0;
}