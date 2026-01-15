// Can't use operands of structure type in compound assignment operations
struct s { int i; };
int main(void) {
    struct s x = {10};
    x += 10;
    return 0;
}