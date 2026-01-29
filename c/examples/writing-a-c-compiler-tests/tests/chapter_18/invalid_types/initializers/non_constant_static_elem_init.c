struct pair {
  int a;
  int b;
};

struct pair x = {1, 2};
struct outer {
    double d;
    struct pair inner;
};

// you can't initialize an element in a static variable with a non-constant expression
struct outer y = {1.0, x};