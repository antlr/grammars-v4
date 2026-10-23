/* Make sure we detect dead stores to union members */

union u {
    long l;
    int i;
};

union u global_union = {10};

int target(void) {
    union u my_union = {4};
    my_union.i = 123; // dead!
    my_union = global_union;
    return my_union.i;
}

int main(void) {
    return target();
}