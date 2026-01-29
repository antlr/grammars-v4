// This test case verifies that we correctly rewrite expressions
// that use static variables; i.e. we recognize that they are memory operands

int main(void) {
    static int i = 2;
    static int j = 3;
    int cmp = i < j; // make sure rewrite cmpl j(%rip), i(%rip)

    if (!cmp)
        return 1;
    return 0;
}