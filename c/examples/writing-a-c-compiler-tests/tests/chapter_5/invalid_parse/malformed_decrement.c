// This is primarily a test for the increment/decrement extra credit feature,
// but the compiler should throw a parse error for this program whether it
// supports that feature or not.
int main(void) {
    int a = 0;
    a - -;
    return a;
}