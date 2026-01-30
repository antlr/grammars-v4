/* You can't negate pointers, including pointers to char */
int main(void) {
    char *x = "foo";
    return -x;
}