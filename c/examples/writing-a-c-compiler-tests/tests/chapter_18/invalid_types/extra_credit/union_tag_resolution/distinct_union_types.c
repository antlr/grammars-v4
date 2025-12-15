int foo(void) {
    union s {
        int a;
        long b;
    };
    union s result = {1};
    return result.a;
}

int main(void) {
    // previously defined union s is not in scope here,
    // so this is declares a new incomplete type
    union s;
    // this is illegal because it defines a variable with an incomplete type
    union s blah = {foo()};
    return blah.a;
}