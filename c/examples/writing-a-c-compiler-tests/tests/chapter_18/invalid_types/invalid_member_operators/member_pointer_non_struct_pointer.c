struct a {
  int x;
  int y;
};

int main(void) {
  struct a my_struct = {1, 2};
  // can't apply -> to non-pointer
  return my_struct->x;
}