int add(int a, int b)
{
    return a + b;
}

int tick()
{
    return 1;
}

int main()
{
    int arr[5];
    int sum;
    sum = add(1, 2);
    arr[0] = sum;
    arr[1] = tick();
    return arr[0];
}
