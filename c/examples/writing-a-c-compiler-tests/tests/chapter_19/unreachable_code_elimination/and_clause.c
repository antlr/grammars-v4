/* Test that we eliminate the second clause in 0 && y */
int putchar(int c);

int target(void) {
    return 0 && putchar(97);
}

int main(void) {
    return target();
}