int main(void)
{
    long x = 10;
    long *ptr = &x + 1;
    long(*array_ptr)[10] = (long (*)[10]) &x;
    // It's illegal to compare two different pointer types
    // without explicitly casting to the same type
    return array_ptr < ptr;
}