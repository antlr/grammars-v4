// Trailing commas are permitted in compound initializers
struct s {
    int a;
    int b;
};

int main(void) {
    struct s x = {
        1,
        2,
    };
    if (x.a != 1 || x.b != 2) {
        return 1;
    }

    return 0;  // success
}