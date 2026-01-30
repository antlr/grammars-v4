// Two members of a union can have the same type
union u {
    int a;
    int b;
};

int main(void) {
    union u my_union = {0};
    my_union.a = -1;
    if (my_union.b != -1){
        return 1; // fail
    }
    return 0; // success
}