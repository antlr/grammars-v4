struct s {
    int x;
    int y;
};

struct t {
    int blah;
    int y;
};

int main(void) {
    struct s foo = {1, 2};
    return foo.blah; // "struct s" has no member "blah"
}