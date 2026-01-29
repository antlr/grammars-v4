struct s;

extern struct s x;
extern struct s y;

int main(void) {
  x = y; // can't assign to or from variable with incomplete type
  return 0;
}