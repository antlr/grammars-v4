int main(void) {
  int arr[3] = {1, 2, 3};
  void *ptr = (void *)arr;
  // you can't compare pointers to different types, e..g. void * to int *
  return ptr < arr + 1;
}