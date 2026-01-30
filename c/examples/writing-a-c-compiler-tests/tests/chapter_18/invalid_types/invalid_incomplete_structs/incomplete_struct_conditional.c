struct s;

extern struct s v1;
extern struct s v2;

int main(void) {
  // can't use expressions with incomplete structure type as branches in conditional expression
  1 ? v1 : v2;
}