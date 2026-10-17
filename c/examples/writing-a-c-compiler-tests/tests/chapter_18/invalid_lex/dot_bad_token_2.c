struct s {
    int a;
};

int main(void) {
    struct s x;
    // Recognize .0foo as an invalid token instead of a struct member operator
    return x.0foo;
}