/* Dead store elimination can also eliminate copies to struct members. */
struct s {
    int i;
};

int f(struct s arg) {
    return arg.i;
}

int target(void) {
    struct s my_struct = {4};
    int x = f(my_struct);
    my_struct.i = 10;  // dead!
    return x;
}

int main(void) {
    return target();
}