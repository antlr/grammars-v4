struct s {
    int a;
};

int main(void) {
    struct s x;
    // we should reject .1l as an invalid preprocessing number;
    // we shouldn't lex it as a dot followed by a valid constant
    return x.1l;
}