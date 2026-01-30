int main(void)
{
    int *y = 0;
    // you can't subtract a double from a pointer
    return (y - 0.0 == 0.0);
}