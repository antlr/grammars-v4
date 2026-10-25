// Can't apply ++/-- to any structures, including nested ones
struct inner {
    int i;
};
struct outer {
    struct inner s;
};
int main(void) {
    struct outer x = {{1}};
    ++x.s;
    return 0;
}