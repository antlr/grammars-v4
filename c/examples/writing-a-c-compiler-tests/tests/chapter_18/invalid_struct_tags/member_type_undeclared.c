struct s {
    // In our implementation, this member declaration fails tag resolution
    // because the 'struct s' type hasn't been declared.
    // In a fully conforming implementation it would fail because it's
    // illegal to declare structure members of incomplete type
    struct a b;
};