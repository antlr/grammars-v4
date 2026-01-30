/* Label names must be unique within a function */
int main(void) {
    int x = 0;
label:
    x = 1;
label:
    return 2;
}