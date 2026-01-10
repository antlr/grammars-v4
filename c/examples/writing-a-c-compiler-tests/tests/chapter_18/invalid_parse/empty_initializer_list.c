// An initializer list must have at least one element
// NOTE: empty initializer lists are valid as of C23

struct s {int a;};

int main(void) {
    struct s foo = {};
    return 0;
}