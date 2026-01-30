struct s; // declare incomplete structure type

struct a {
  // can't declare a struct member with incomplete type
  struct s g;
};