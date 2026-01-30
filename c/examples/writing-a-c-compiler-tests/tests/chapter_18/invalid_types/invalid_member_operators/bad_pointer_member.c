void *malloc(unsigned long size);

struct a {
  int x;
  int y;
};

struct b {
  int m;
  int n;
};

int main(void) {
  struct a *ptr = malloc(sizeof(struct a));
  ptr->m = 10; // "struct a" has no member "m"
  return 0;
}
