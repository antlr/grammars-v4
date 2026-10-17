// You can't use a character constant as a label in a goto statement
int main(void) {
    goto 'x';
    'x';
    return 0;
}