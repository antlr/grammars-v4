// Can't use structure as controlling expression in switch statement
struct s {
    int i;
};

int main(void) {
    struct s x = {1};
    switch (x) {
        case 1:
            return 0;
        default:
            return 1;
    }
}