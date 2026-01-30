// Can't apply %= to a double
int main(void) {
    int i = 5;
    i %= 1.0;
    return i;
}