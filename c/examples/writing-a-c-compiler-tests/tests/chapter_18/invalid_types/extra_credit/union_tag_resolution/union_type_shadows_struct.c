// Test that we correctly track unions shadowing structs w/ same tag

// define a struct type
struct u {
    int a;
};

int main(void) {
    // declare an incomplete union type shadowing earlier complete type
    union u;
    union u my_union; // invalid - type is incomplete
    return 0;
}