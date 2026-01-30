// The result of a compound assignment expression through a pointer isn't an
// lvalue, so you can't take its address with &
int main(void) {
    int i = 100;
    int *ptr = &i;
    int *ptr2 = &(*ptr -= 10);
    return 0;
}