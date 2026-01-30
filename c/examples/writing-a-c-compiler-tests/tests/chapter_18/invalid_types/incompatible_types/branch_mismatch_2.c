struct s {
    int a;
};

int main(void) {
    struct s x = {1};
    // can't have conditional branches where only one branch is a struct
    1 ? x : (void) 2;
}