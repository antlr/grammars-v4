// Can't cast union to scalar type even if it has the right size
union u {
    int i;
};

int main(void) {
    union u x = {10};
    return (int)x;
}