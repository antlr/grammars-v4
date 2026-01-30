// You can't use a string literal as a label in a goto statement
int main(void) {
    goto "foo";
    return 0;
}