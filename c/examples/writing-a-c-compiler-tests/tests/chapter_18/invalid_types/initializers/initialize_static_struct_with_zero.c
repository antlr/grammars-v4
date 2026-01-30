struct s {
    int a;
};

// you can't initialize a static struct (or any struct) with a scalar constant
struct s x = 0;