struct s {
    int x;
    // a structure can't contain a member with its own type,
    // because its type has not yet been completed.
    // here, 'struct s' is still an incomplete type
    struct s y;
};

int main(void) {
    return 0;
}