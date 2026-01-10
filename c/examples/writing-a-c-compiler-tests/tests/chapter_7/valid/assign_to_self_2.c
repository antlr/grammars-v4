int main(void) {
    int a = 3;
    {
        int a = a = 4;
    }
    return a;
}