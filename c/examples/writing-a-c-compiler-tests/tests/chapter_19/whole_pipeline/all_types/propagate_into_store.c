/* Make sure we can propagate copies into Store instruction */

struct s {
    int a;
    int b;
};

int glob = 0;
int i = 0;
int target(void) {
    int *ptr = &i;
    glob = 30;  // this can be removed once we propagate its value

    *ptr = glob;  // rewrite as *ptr = 30, letting us remove
                  // previous write to glob

    glob = 10;
    return *ptr;
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