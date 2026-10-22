// Can't apply postfix ++/-- to void lvalue
extern void *x;

int main(void) {
    ++(*x)--;
    return 0;
}