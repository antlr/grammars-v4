struct s1 {
    int a;
};

struct s2 {
    int a;
};

int get_a(struct s1 *ptr) {
    return ptr->a;
}

int main(void) {
    struct s2 arg = {1};
    // can't pass a struct s2 * to a function that expects a struct s1 *
    return get_a(&arg);
}