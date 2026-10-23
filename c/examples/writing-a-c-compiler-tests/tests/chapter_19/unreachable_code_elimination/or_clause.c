/* Test that we eliminate the second clause in 1 || x */
int putchar(int c);

int target(void) {
    return 1 || putchar(97);
}

int main(void) {
    return target();
}