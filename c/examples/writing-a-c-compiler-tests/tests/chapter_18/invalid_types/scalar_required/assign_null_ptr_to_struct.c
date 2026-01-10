struct s {
  int a;
};

struct s x = {1};

int main(void) {
  // can't assign any scalar value (including null pointer constant)
  // to an lvalue of struct type
  x = 0;
  return 0;
}