// recursive fibonacci
int fib(int n)
{
    /* base cases */
    if (n < 2)
        return n;
    return fib(n - 1) + fib(n - 2);
}

int main()
{
    return fib(10);
}
