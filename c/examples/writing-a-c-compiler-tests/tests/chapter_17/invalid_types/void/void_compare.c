int main(void) {
  // you can't compare void expressions
  if ((void)1 < (void)2)
    return 1;
  return 0;
}