int main(void) {
  int arr[3] = {1, 2, 3};
  void *void_ptr = arr;
  int *int_ptr = arr + 1;
  // result of conditional is void *
  // can't subscript a pointer to an incomplete type
  // (although Clang/GCC let you subscript void * as a language extension)
  return (1 ? int_ptr : void_ptr)[1];
}