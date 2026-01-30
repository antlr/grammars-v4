int sub(int a, int b) {
    /* Make sure arguments are passed in the right order
     * (we can test this with subtraction since a - b  != b - a)
     */
    return a - b;
}

int main(void) {
    /* Make sure we can evaluate expressions passed as arguments */
    int sum = sub(1 + 2, 1);
    return sum;
}
