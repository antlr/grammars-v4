struct pair {
    int a;
    int b;
};

int main(void) {
    // you can't use the dot operator to specify one member in a struct
    // declarator; you need a compound initializer to initialize structure
    // members
    struct pair x.a = 10;
}