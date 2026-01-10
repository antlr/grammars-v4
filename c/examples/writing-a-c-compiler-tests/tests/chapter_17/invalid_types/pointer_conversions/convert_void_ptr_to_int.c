int main(void) {
  void *x = 0;
  return x; // can't implicitly convert void * to integer as if by assignment
}