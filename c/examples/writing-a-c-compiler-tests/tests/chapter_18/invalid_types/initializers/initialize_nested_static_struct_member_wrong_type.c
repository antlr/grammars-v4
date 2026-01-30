struct s {
  double d;
  void *arr[3];
};

// can't initialize a nested element of type void * with a constant of type double
struct s x = {0.0, {1.0}};