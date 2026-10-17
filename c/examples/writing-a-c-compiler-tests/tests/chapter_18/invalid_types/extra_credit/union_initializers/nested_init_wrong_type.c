/* The one element in a union initializer (including a nested one)
 * must be compatible with the union's first element
 */

union u {
    double d;
    int i;
    char c;
};

struct s {
    int *ptr;
    union u arr[3];
};

int main(void) {
    int x;

    // invalid initializer for last element of arr;
    // can't convert pointer &x to double
    struct s my_struct = {&x, {{1.0}, {2.0}, {&x}}};
}