/* You can't initialize a pointer with a compound initializer */
int main(void) {
    char *ptr = {'a', 'b', 'c'};
    return 0;
}