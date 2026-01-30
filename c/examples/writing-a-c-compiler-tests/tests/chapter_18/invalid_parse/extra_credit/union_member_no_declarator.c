union u {
    int;  // every union member needs a declarator
    // we treat this as a parse error but catching it in the type checker would
    // also be reasonable
    // NOTE: a union member with anonymous struct or
    // union type doesn't need a declarator (but we don't support anonymous
    // structs/unions)
};