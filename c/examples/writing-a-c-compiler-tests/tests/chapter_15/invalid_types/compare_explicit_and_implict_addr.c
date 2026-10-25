int main(void)
{
    int arr[10];
    /* It's illegal to compare these expressions because
     * they have different types: arr is converted to type int *,
     * while &arr will have type int (*)[10]
     */
    return arr == &arr;
}