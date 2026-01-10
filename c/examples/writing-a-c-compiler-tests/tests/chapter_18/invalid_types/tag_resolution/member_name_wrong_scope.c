struct s {
    int a;
};

int main(void) {
    struct s foo = {1};

    // introduce a different struct s type
    struct s {
        int b;
    };

    return foo.b; // foo belongs to outer struct s type, which doesn't have member 'b'
}