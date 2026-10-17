void *malloc(unsigned long size);

union a {
  int x;
  int y;
};

union b {
  int m;
  int n;
};

int main(void) {
  union a *ptr = malloc(sizeof(union a));
  ptr->m = 10; // "union a" has no member "m"
  return 0;
}
