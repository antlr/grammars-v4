int main(void)
{
    // you can't initialize a static array with a scalar, not even a null pointer constant
    static int arr[1] = 0;
    return arr[0];
}