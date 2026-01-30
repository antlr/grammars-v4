struct s {
    int a;
};
int main(void) {
    // struct member operator (.) can be separate by whitespace from
    // both struct and field name
    struct s foo;
    foo .a = 10;
    int b = foo .a;

    return foo . a == b;
}