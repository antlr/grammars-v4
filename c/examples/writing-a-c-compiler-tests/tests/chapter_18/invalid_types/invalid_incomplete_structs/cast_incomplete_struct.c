struct s;

extern struct s v;

int main(void) {
  // you can't perform a cast on a struct with incomplete type
  (void)v;
  return 0;
}