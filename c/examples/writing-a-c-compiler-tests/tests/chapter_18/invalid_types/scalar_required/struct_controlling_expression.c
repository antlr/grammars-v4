struct s {
  int a;
};

int main(void) {
  struct s x = {1};
  // can't use structure as controlling expression in if statement
  if (x)
    return 1;
  return 0;
}