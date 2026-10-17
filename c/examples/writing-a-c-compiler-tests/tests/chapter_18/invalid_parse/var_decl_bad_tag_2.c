struct s {
    int y;
};

int main(void) {
    // can't parenthesize struct tag
    struct(s) var;

    return 0;
}