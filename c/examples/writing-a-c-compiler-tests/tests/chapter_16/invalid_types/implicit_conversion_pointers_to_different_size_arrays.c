/* A pointer to a string can only be assigned to a pointer to an array
 * with the correct length. */

int main(void) {
    // &"x" has type char (*)[2],
    // so it can't initialize a variable of type char (*)[10]
    char(*string_pointer)[10] = &"x";
    return 0;
}