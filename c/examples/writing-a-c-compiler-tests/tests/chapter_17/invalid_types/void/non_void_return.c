void x(void) {
  // a function with a void return type can't return an expression
  return 1;
}

int main(void) {
  x();
  return 0;
}