struct s;

extern struct s foo;

int main(void) {
  return foo.a; // can't get member of incomplete structure type
}