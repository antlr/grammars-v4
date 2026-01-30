int main(void) {
    int x = 1;
    a:
    x = x + 1;
    a(); // can't call a label like a function
    return x;

}