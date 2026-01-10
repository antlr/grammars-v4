struct s {
  int arr[3];
  double d;
};

int main(void) {
  struct s x = {{1, 2, 3}, 4.0};
  struct s y = {{9, 8, 7}, 6.0};
  // can't take address of element b/c it's not an lvalue
  // (even though it has temporary lifetime)
  int *arr[3] = &((1 ? x : y).arr);
  return 0;
}