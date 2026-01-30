struct s {
  int a;
};

int main(void) {
  struct s x = {1};
  // can only apply ~ operator to ints, not structs
  (void)~x;
  return 0;
}