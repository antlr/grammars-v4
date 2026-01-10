int main(void)
{
    int *x = 0;
    // you can't subtract a pointer from an integer
    // Note that 0 is NOT implicitly converted to a null pointer here
    return 0 - x == 0;
}