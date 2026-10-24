int main(void) {
    // different labels do not define different scopes
label1:;
    int a = 10;
label2:;
    int a = 11;
    return 1;
}