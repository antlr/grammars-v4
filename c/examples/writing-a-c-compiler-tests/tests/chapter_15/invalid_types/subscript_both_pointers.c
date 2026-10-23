int main(void)
{
    int x = 10;
    int *ptr = &x;
    int *subscript = 0;
    // you can't perform subscript operation when both operands are pointers
    return ptr[subscript];
}