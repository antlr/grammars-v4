int main(void) {
    /* Function declarations aren't permitted in for loop headers. */
    for (int f(void); ; ) {
        return 0;
    }
}