struct s {
  int arr[3];
  double d;
};

int main(void) {
  struct s x = {{1, 2, 3}, 4.0};
  struct s y = {{9, 8, 7}, 6.0};
  // can't assign to this struct member because it's not an lvalue
  (1 ? x : y).d = 0.0;

  return 0;
}