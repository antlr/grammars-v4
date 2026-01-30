int main(void)
{
    // invalid implicit conversion of double 1.0 to type int *
    int *arr[3] = {0, 0, 1.0};
}