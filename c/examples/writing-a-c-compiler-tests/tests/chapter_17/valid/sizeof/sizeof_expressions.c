/* Test that we correctly get the size of a range of expressions */

#if defined SUPPRESS_WARNINGS && defined __clang__
#pragma clang diagnostic ignored "-Wunevaluated-expression"
#endif

void *malloc(unsigned long size);
void free(void *ptr);

int main(void) {
    // size of variables

    double d;

    if (sizeof d != 8) {
        return 2;
    }

    unsigned char c;

    if (sizeof c != 1) {
        return 3;
    }

    void *buffer = malloc(100);

    // sizeof(buffer) gets the size of the pointer, not the buffer itself
    if (sizeof(buffer) != 8) {
        return 4;
    }

    free(buffer);

    // more complex expressions

    // sizeof (int) d is a syntax error,
    // but applying sizeof to a parenthesized cast expression is ok
    if (sizeof ((int)d) != 4) {
        return 5;
    }

    // result type is long
    if (sizeof (d ? c : 10l) != 8) {
        return 6;
    }

    // result type is char

    if (sizeof (c = 10.0) != 1) {
        return 7;
    }

    return 0;
}