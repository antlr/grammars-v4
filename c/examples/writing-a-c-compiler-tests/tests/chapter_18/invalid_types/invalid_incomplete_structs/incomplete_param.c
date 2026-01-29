struct s;

// it's illegal to define a function with a parameter of incomplete type,
// even if the parameter isn't used
int foo(struct s x) { return 0; }
