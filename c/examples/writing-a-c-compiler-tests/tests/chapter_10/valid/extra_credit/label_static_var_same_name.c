// a static variable and label within the same function can share a name
// (make sure we don't e.g. use the naming scheme "main.x" in both cases)
int main(void) {
    static int x = 5;
    goto x;
    x = 0;
x:
    return x; // return 5
}