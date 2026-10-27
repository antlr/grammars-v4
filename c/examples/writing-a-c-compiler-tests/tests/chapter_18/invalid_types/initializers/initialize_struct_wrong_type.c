struct one {
  int x;
  int y;
};

struct two {
  int a;
  int b;
};

int main(void) {
  struct one x = {1, 2};
  struct two y = x; // can't initialize a struct from different struct type
  return 0;
}