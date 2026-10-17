void f(void) { return; }
int main(void) {
  int i = 0;
  // void expressions are non-scalar, so they can't be used as controlling conditions
  while ((void)10) {
    i = i + 1;
  }
  return 0;
}