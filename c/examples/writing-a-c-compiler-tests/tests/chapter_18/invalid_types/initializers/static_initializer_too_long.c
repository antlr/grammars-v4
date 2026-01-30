struct pair {
    int a;
    int b;
};
// a static compound structure initializer can't initialize more values than the
// struct has
struct pair p = {1, 2, 3};