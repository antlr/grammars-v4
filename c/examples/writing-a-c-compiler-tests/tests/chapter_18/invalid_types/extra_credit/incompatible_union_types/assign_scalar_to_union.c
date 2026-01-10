// Can't assign scalar value to union, even if value has same type
// as first element
union u {int a; int b;};

int main(void) {
    union u x = {1};
    x = 2; // invalid
    return 0;
}