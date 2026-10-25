int main(void)
{
    long x[10];
    long *ptr = x;
    unsigned long *ptr2 = (unsigned long *)ptr;
    // You can't subtract pointers to different types
    return ptr - ptr2;
}