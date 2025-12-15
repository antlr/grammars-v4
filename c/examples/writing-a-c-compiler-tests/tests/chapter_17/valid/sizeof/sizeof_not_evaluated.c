void exit(int status);
int foo(void) { exit(10); }

int main(void) {
  // make sure foo isn't actually called
  return sizeof(foo());
}