/* It's illegal to return an int *
 * from a function with return type long *
 * because you can't implicitly convert
 * one pointer type to another
 */
int i;

long *return_long_pointer(void) {
    return &i;
}

int main(void) {
    long *l = return_long_pointer();
    return 0;
}