struct s {
  int a;
};

int main(void) {
  struct s x = {1};
  // can't cast struct to a scalar value
  int y = (int)x;
  return y;
}