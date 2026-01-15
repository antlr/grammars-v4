struct s {
    int a;
};

int main(void) {
    struct s x = {1};
    return !x;  // can only apply boolean operators to scalars, not structs
}