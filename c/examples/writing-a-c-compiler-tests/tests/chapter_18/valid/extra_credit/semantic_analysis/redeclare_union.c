// Redeclaring a union that was already defined in the current scope has no effect
int main(void) {
    union u {
        int a;
    };

    union u; // this does nothing

    union u my_union = {1};
    return my_union.a;
}