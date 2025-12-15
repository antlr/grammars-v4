int a = 10;
/* b has static storage duration,
 * so its initializer must be constant.
 */
int b = 1 + a;

int main(void) {
    return b;
}