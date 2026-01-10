struct s {
  int a;
};

int main(void) {
  struct s x = {1};
  return x[0]; // can only subscript pointers, not structures
}