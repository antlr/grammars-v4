long a;

int main(void) {
    /* This declaration refers to the global 'a' variable,
     * but has a conflicting type.
     */
    extern int a;
    return 0;
}