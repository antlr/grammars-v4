int main(void)
{
    int arr[3] = {1, 2, 3};
    int arr2[3] = {4, 5, 6};
    // arr has array type, so it can't be assigned to
    arr = arr2;
    return arr[0];
}