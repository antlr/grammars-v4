struct s {
    int i;
    char arr[2];
    double d;
};

struct outer {
    char c;
    struct s inner;
};

extern struct s global;
extern struct outer global_outer;

void update_struct(void);
void update_outer_struct(void);