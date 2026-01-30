// Like structures, unions can appear in conditional expression

union u {
    long l;
    int i;
    char c;
};
int choose_union(int flag) {
    union u one;
    union u two;
    one.l = -1;
    two.i = 100;

    return (flag ? one : two).c;
}

int main(void) {
    if (choose_union(1) != -1) {
        return 1; // fail
    }

    if (choose_union(0) != 100) {
        return 2; // fail
    }

    return 0; // success
}