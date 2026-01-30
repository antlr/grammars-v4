// Example from "Test the Semantic Analysis Stage" box
// intended to test identifier resolution

struct s;
struct s *ptr1 = 0;
int main(void) {
  struct s;
  struct s *ptr2 = 0;
  return ptr1 == ptr2;
}
