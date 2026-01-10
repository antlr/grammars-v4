int main(void) {
  // can't convert void * to int with usual arithmetic conversions
  int i = 10 * (void *)0;
}