int main(void)
{
    // You can't initialize an array with a scalar, not even a null pointer constant
    int arr[1] = 0;
    return arr[0];
}