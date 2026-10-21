struct chars {
  char char_array[5];
};

int main(void) {
  struct chars x = {{1, 2, 3, 4, 5}};
  char arr[5] = {9, 8, 7, 6, 5};
  // can't assign to char_array member because it decays to a pointer
  x.char_array = arr;
  return x.char_array[0];
}