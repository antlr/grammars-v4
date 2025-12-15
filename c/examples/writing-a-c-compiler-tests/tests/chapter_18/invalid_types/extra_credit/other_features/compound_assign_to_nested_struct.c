// Can't perform compound assignment with lval of struct type, including
// nested structs

struct inner {
    int i;
};

struct outer {
    struct inner s;
};

int main(void) {
    struct outer x = {{1}};
    x.s *= 10;
    return 0;
}