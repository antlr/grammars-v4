int main(void) {
    struct pair {
        int x;
        int y;
    };

    // you can't initialize a struct with a scalar expression
    struct pair p = 1;
}