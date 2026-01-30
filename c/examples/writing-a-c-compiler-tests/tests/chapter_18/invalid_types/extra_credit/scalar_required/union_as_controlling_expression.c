// Can't use union as controlling expression

union u {int x;};

int main(void) {
    union u my_union = {10};
    if (my_union) {
        return 1;
    }
    return 0;
}