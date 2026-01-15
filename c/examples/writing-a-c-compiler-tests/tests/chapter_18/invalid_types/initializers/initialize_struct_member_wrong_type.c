struct s {
    signed char *char_ptr;
};

int main(void) {
    // because the structure member char_ptr has type signed char *,
    // rather than char *, we can't initialize it with a string literal
    struct s x = {"It's a string"};
    return 0;
}