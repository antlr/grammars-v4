// Can't use a structure type as the right operand in compound assignment
struct s { int i; };
int main(void) {
    int i = 100;
    struct s x = { 100 };
    i += x;
    return 0;
}