// An initializer list must have at least one element
// NOTE: empty initializer lists are valid as of C23
union u { int a; };

int main(void) {
    union u x = {};
}