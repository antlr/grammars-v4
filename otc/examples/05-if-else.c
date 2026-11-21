int max(int a, int b)
{
    if (a < b)
        return b;
    else
        return a;
}

int sign(int n)
{
    if (n == 0)
        return 0;
    else if (n < 0)
        return 0 - 1;
    else
        return 1;
}
