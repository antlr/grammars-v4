// Test updating a static local variable over multiple function invocations;
// also test passing a static variable as an argument
int putchar (int ch);

int print_alphabet(void) {
    /* the value of count increases by 1
     * each time we call print_alphabet()
     */
    static int count = 0;
    putchar(count + 65); // 65 is ASCII 'A'
    count = count + 1;
    if (count < 26) {
        print_alphabet();
    }
    return count;
}

int main(void) {
    print_alphabet();
}