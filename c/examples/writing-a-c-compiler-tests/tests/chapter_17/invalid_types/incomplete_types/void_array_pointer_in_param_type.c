// array of incomplete element type is illegal
int foo(void (*bad_array)[3]) {
    return bad_array == 0;
}

int main(void) {
    return 0;
}