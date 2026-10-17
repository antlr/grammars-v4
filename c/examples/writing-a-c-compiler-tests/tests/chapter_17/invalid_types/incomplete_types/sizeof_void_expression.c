int main(void) {
  int x;
  // can't apply sizeof to an expression with incomplete type
  return sizeof((void)x);
}