int foo(int a, int b, int c);
int main(void) {
    int arr[3] = {
        1,
        2,
        3, // last element in a compound initializer may have a trailing comma
    };
    return arr[2];
}
