struct s {
  int a;
};

struct s x;

// can only cast to scalar type or void
// casting to struct type is illegal,
// even if operand already has that type
// (Clang/GCC only complain about this with -pedantic option)
int main(void) { (struct s) x; }