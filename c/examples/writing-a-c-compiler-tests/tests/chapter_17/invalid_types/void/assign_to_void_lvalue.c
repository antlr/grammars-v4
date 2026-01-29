extern void *x;

void foo(void) { return; }

int main(void) {
  // in our implementation, you can't dereference void *
  // the standard is ambiguous on whether this is legal,
  // but you definitely can't dereference it and then assign to the result
  *x = foo();
  return 0;
}