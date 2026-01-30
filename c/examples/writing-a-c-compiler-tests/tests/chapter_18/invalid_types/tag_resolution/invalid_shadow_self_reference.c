struct s {
    int a;
};

int main(void) {
    struct s {
        // it's illegal to declare a member of type 'struct s' here, because tag
        // 's' refers to the type we're declaring now instead of the type we
        // declared earlier (this tests that we add the new tag to current scope before we
        // process its members)
        struct s nested;
    };
    return 0;
}