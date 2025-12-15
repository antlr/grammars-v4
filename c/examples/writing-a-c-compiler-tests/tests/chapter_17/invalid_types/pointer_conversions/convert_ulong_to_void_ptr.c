int main(void) {
  unsigned long x = 0;
  void *v = x; // can't implicitly convert a non-pointer type to a pointer
}