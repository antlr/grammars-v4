int x(void);

int main(void) {
    int x = 0;
    /* x isn't a function, so you can't call it */
    return x();
}