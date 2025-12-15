int main(void) {
    // In our implementation, this fails tag resolution because 'union c'
    // hasn't been declared yet.
    // In a fully conforming implementation it would fail because you
    // can't apply sizeof to incomplete types.
    return sizeof(union c);
}

union c {
    int x;
};