struct s1 {
  int a;
};

struct s2 {
  int b;
};

int main(void) {
  struct s1 x = {1};
  struct s2 y = {2};
  1 ? x : y; // can't have conditional branches with different struct types
}