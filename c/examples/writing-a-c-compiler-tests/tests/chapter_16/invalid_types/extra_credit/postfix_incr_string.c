// can't apply postfix ++/-- to string literals
int main(void) {
    "foo"++;
    return 0;
}