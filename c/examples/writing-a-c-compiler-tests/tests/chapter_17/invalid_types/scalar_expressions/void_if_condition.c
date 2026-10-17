int main(void) {
  int x = 10;

  // void expressions are non-scalar, so they can't be used as controlling conditions
  if ((void)x)
    return 0;
  return 1;
}