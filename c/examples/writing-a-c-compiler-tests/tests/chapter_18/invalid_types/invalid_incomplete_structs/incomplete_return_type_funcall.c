struct s;

struct s f(void);

int main(void) {
  f(); // can't call a function with an incomplete return type (besides void)
  return 0;
}