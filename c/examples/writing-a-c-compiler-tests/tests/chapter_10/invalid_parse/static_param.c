/* A function parameter cannot have a storage class */
int f(static int i) {
    return i;
}

int main(void) {
    return f(1);
}