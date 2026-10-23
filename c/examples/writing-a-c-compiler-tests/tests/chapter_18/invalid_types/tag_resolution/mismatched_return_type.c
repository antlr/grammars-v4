struct s {
  int a;
  int b;
};

struct s return_struct(void) {
  // define another struct s that shadows previous one;
  struct s {
    int a;
    int b;
  };
  struct s result = {1, 2};
  // result has inner 'struct s' type instead of outer one,
  // so it's incompatible w/ function's return type
  return result;
}
