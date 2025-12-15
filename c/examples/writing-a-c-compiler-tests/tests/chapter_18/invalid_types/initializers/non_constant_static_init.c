struct pair {
  int a;
  int b;
};
struct pair x = {1, 2};
// you can't initialize a static variable with a non-constant expression
struct pair y = x;