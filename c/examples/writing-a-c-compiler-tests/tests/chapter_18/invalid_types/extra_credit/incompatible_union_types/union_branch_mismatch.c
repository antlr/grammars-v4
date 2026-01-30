// If either branch of a conditional has union type, branches must have
// identical types
int main(void) {
    union u1 {
        int a;
    };
    union u2 {
        int a;
    };
    union u1 x = {10};
    union u2 y = {11};
    1 ? x : y; // mismatch: x and y have different types
    return 0;
}