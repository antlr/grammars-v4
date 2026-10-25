// Can't combine union specifier with other type specifier

union a { int a; };

int main(void) {
    union a int x;
    return 0;
}