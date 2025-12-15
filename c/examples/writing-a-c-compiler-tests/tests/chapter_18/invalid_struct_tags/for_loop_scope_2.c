// a struct tag declared in a for loop body is not visible in the loop header

int main(void) {
    void *ptr;

    // there's no 'struct s' tag in scope in this loop's post-expression
    // in our implementation, this will fail tag resolution.
    // in a fully-conforming implementation, it would fail because it's
    // attempting to access a member of an incomplete type
    for (;; ((struct s *)ptr)->i) {
        struct s {
            int i;
        };
        struct s x = {1};
        ptr = &x;
    }
}