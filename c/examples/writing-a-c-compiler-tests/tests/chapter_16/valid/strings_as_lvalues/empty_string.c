/* Test that we add a terminating null byte to the empty string */
int main(void) {
    char *empty = "";
    return empty[0];
}