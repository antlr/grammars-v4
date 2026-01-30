struct s {
    int E10;
};

int main(void) {
    struct s x1 = {3};
    return x1.E10;  // lex correctly, recognizing that 1.E10 is not a constant
}
