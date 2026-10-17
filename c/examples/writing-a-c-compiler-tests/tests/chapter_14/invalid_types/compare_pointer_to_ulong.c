/* It's illegal to compare a pointer to a value with a different type */
int main(void) {
    int *ptr = 0ul; // Initializing a pointer with a null constant is allowed
    unsigned long ul = 0ul;
    return ptr == ul;
}