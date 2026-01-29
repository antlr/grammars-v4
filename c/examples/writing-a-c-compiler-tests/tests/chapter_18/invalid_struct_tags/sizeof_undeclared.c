int main(void) {
    // In our implementation, this fails tag resolution because 'struct c'
    // hasn't been declared yet.
    // In a fully conforming implementation it would fail because you
    // can't apply sizeof to incomplete types.
    return sizeof(struct c);
}

struct c {
    int x;
};