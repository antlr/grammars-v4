extern void v1;

int main(void) {
  // in our implementation, you can't declare void variables
  // the standard is ambiguous about whether this is legal,
  // but you definitely can't declare a void variable and assign to it
  v1 = (void)0;
  return 0;
}