int sum(int n)
{
    int total;
    int i;
    total = 0;
    i = 0;
    while (i < n)
    {
        total = total + i;
        i = i + 1;
    }
    return total;
}

int countdown(int n)
{
    do
    {
        n = n - 1;
    }
    while (n != 0);
    return n;
}
