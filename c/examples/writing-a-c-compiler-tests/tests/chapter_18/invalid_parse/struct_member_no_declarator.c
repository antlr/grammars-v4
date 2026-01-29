struct s {
    int;  // every structure member needs a declarator
    // we treat this as a parse error but catching it in the type checker would
    // also be reasonable
    // NOTE: a structure member with anonymous struct or
    // union type doesn't need a declarator (but we don't support anonymous
    // structs/unions)
};