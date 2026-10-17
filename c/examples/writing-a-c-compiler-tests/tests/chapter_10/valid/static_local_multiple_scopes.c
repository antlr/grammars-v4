/* static local variables declared in different scopes
 * in the same function are distinct from each other.
 */

int putchar (int ch);

int print_letters(void) {
    /* declare a static variable, initialize to ASCII 'A' */
    static int i = 65;
    /* print the ASCII character for its current value */
    putchar(i);
    {
        /* update the outer static 'i' variable */
        i = i + 1;

        /* declare another static variable, initialize to ASCII 'a' */
        static int i = 97;
        /* print the ASCII character for inner variable's current value */
        putchar(i);
        /* increment inner variable's value */
        i = i + 1;
    }
    /* print a newline */
    putchar(10);
    return 0;
}

int main(void) {
    //print uppercase and lowercase version of each letter in the alphabet
    for (int i = 0; i < 26; i = i + 1)
        print_letters();
}