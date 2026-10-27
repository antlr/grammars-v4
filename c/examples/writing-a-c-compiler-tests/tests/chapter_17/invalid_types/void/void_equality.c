void x(void);
int main(void) {
    // you can't compare void expressions
    return x() == (void)10;
}