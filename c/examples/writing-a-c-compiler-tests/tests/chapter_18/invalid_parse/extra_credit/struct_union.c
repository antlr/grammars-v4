// Can't use struct and union keywords in same declaration
union struct s {
    int a;
};

int main(void) {
    return 0;
}