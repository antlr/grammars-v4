struct s {
  int a;
};
int foo(int a) { return a; }

int main(void) {
  struct s x = {1};
  // can't convert struct to scalar (or to any other type) as if by assignment
  return foo(x);
}