/* A declaration cannot include both static and extern specifiers */
static extern int a;

int main(void) {
    return 0;
}