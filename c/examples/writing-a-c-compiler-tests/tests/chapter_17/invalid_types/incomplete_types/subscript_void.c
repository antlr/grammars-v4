// you can't subscript pointers to incomplete types
// although Clang/GCC let you subscript void * as a language extension
int main(void) {
  int x = 10;
  void *v = &x;
  v[0];
  return 0;
}