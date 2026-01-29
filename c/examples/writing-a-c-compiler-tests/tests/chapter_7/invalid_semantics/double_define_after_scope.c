int main(void) {
    int a = 3;
    {
        a = 5;
    }
    int a = 2;
    return a;
}