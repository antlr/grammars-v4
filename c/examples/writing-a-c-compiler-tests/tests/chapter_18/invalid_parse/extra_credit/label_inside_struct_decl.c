/* Labels can't appear inside structure declarations */

struct s {
    int i;
    // NOTE: GCC and clang will treat foo as a bit-field (a feature we aren't
    // implementing. If you implement bit-fields, this is still a parse error
    foo : int j;
};

int main(void) {
    return 0;
}