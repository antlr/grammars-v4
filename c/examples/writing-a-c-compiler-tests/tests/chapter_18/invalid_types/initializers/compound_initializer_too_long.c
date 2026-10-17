struct pair {
    int a;
    int b;
};

int main(void) {
    // a compound structure initializer can't initialize more values than the
    // struct has
    struct pair p = {1, 2, 3};
    return 0;
}