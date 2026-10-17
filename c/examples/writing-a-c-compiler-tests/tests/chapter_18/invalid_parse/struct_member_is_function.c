struct s {
    // a structure member can't be a function
    // we treat this as a parse error, but it would also be reasonable
    // to handle it during type checking
    int foo(void);
};