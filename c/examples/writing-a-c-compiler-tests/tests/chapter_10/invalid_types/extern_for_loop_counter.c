int main(void) {

    int x = 0;

    /* a variable declared in a for loop header cannot have a storage class. */
    for (extern int i = 0; i < 10; i = i + 1) {
        x = x + 1;
    }

    return x;
}