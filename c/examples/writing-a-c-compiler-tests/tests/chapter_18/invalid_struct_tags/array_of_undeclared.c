int main(void) {
    // In our implementation, this declaration fails tag resolution because the
    // 'struct s' type hasn't been declared.
    // In a fully conforming implementation it would fail because it's illegal
    // to specify arrays of incomplete type
    struct s arr[2];
    return 0;
}