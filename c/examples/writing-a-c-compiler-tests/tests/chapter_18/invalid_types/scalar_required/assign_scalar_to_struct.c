struct s {
  int a;
};

struct s x = {1};

int main(void) {
  struct s *ptr = &x;
  *ptr = 2; // can't assign scalar value to lvalue of struct type
  return 0;
}