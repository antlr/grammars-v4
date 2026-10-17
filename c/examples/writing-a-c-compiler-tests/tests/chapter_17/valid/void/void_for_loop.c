/* Test for void expressions in for loop header */

int putchar(int c);  // from standard library

int letter;
void initialize_letter(void) {
    letter = 'Z';
}

void decrement_letter(void) {
    letter = letter - 1;
}

int main(void) {
    // void expression in initial condition: print the alphabet backwards
    for (initialize_letter(); letter >= 'A';
         letter = letter - 1) {
        putchar(letter);
    }

    // void expression in post condition: print the alphabet forwards
    for (letter = 'A'; letter <= 90; (void)(letter = letter + 1)) {
        putchar(letter);
    }

    // void expressions in both conditions: print the alphabet backwards again
    for (initialize_letter(); letter >= 65; decrement_letter()) {
        putchar(letter);
    }
    return 0;
}