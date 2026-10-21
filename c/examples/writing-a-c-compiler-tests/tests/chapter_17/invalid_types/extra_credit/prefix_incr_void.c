// Can't apply prefix ++/-- to void lvalue
extern void *x;

int main(void) {
    ++(*x);
    return 0;
}