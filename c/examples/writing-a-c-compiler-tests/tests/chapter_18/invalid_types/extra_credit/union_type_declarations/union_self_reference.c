union u {
    int i;
    union u self; //illegal; incomplete member type
};

int main(void) {
    return 0;
}