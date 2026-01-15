struct s {
    int a;
};

union u {
    struct s nested;
};

int main(void) {
    union u my_union = {{1}};
    // need to specify member name 's' even though it's only member
    return my_union.a;
}