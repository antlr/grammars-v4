int main(void) {
    union u1;
    union u2;

    union u1 *ptr1 = 0;
    union u2 *ptr2 = 0;

    1 ? ptr1 : ptr2; // INVALID: different pointer types
    return 0;
}