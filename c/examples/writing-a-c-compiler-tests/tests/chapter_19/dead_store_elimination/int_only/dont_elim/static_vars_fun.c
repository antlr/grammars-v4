/* Test that we recognize that function calls generate all static variables */
int x = 100;

int get_x(void) {
    return x;
}

int main(void) {
    x = 5;  // don't eliminate this!
    int result = get_x();
    x = 10;
    return result;
}