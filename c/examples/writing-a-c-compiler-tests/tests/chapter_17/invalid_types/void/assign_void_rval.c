/* can't convert void to another type by assignment */
int main(void) {
  int a = 10;
  a = (void)20;
  return 0;
}