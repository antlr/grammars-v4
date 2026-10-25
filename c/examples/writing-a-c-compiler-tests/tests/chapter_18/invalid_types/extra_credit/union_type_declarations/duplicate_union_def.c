/* YOu can't declare the same union type twice. */

int main(void) {
    union u {int a;};
    union u {int a;}; // illegal - duplicate declaration
    return 0;
}