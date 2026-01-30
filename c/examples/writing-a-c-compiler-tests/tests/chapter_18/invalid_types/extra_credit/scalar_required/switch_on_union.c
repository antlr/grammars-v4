// Can't use union as controlling expression in switch statement
union s {
    int i;
};

int main(void) {
    union s x = {1};
    switch (x) {
        case 1:
            return 0;
        default:
            return 1;
    }
}