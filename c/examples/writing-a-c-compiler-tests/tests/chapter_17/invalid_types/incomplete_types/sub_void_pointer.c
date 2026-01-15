// no pointer arithmetic with pointers to incomplete type
// (GCC/Clang allow this as an extension)

int main(void) {
  int y;
  void *x = &y;
  void *null = 0;
  return x - null;
}