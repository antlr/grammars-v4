/* Postfix operators have higher precedence than prefix */

int main(void) {
    int a = 1;
    int b = !a++;
    return (a == 2 && b == 0);
}