/* Basic test of passing an argument of structure type */
struct pair {
    int x;
    double y;
};

// pass x in EDI and y and XMM0
double test_struct_param(struct pair p) {
    if (p.x != 1 || p.y != 2.0) {
        return 0;
    }

    return 1;  // success
}

int main(void) {
    struct pair x = {1, 2.0};
    if (!test_struct_param(x)) {
        return 1;
    }
    return 0;  // success
}