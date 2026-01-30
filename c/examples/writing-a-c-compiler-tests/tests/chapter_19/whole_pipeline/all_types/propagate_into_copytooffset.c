/* Make sure we can propagate copies into CopyToOffset instruction */

struct s {
    int a;
    int b;
};

int glob = 0;

int target(void) {
    struct s my_struct = {1, 2};

    glob = 30;  // this can be removed once we propagate its value

    my_struct.b = glob;  // rewrite as my_struct.b = 30, letting us remove
                         // previous write to glob

    glob =
        10;  // glob is dead since we update it before returning from function
    return my_struct.b;
}

int main(void) {
    if (target() != 30) {
        return 1;  // failure
    }
    if (glob != 10) {
        return 2;  // failure
    }
    return 0;  // success
}