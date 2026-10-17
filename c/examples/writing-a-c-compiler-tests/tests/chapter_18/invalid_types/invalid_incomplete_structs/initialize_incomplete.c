struct s;

// you can declare extern variables of incomplete type
// but it's illegal to initialize them
extern struct s x = {1};

int main(void) { return 0; }

struct s {
  int a;
};